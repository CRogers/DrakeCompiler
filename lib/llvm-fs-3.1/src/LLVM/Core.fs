module LLVM.Core

open System.Runtime.InteropServices
open System.Threading

open LLVM.Generated.Core
open LLVM.FFIUtil

/// Extends BuilderRef. This version implements IDisposable and will call
/// disposeBuilder on itself automatically
type Builder(builderRef : BuilderRef) =
    inherit BuilderRef(builderRef.Ptr)
    
    [<VolatileField>]
    let mutable disposed = 0

    new() = new Builder(createBuilder ())

    new(ctxt : ContextRef) = new Builder(createBuilderInContext ctxt)
    
    new(bb : BasicBlockRef) as x =
        new Builder(createBuilder ()) then
        positionBuilderAtEnd x bb
    
    interface System.IDisposable with
        member x.Dispose () =
            if Interlocked.CompareExchange(&disposed, 1, 0) = 0 then
                disposeBuilder x

let varArgFunctionType (retTy : TypeRef) (paramTys : TypeRef array) =
    use paramPtrs = new NativePtrs([|for pt in paramTys -> pt.Ptr|])
    let paramCount = uint32 paramTys.Length
    
    TypeRef (functionTypeNative (retTy.Ptr, paramPtrs.Ptrs, paramCount, true))

let functionType (retTy : TypeRef) (paramTys : TypeRef array) =
    use paramPtrs = new NativePtrs([|for pt in paramTys -> pt.Ptr|])
    let paramCount = uint32 paramTys.Length
    
    TypeRef (functionTypeNative (retTy.Ptr, paramPtrs.Ptrs, paramCount, false))

let getBasicBlocks (f : ValueRef) =
    let bbCount = countBasicBlocks f
    if bbCount >= 1u then
        let rec goNext i bb =
            if i < bbCount then
                let nextBB = getNextBasicBlock bb
                bb :: goNext (i + 1u) nextBB
            else
                [bb]
        goNext 1u (getFirstBasicBlock f)
    else
        []

let buildCall (bld : BuilderRef) (func : ValueRef) (args : ValueRef array) (name : string) =
    use argPtrs = new NativePtrs([|for vr in args -> vr.Ptr|])
    let argCount = uint32 args.Length
    
    ValueRef (buildCallNative (bld.Ptr, func.Ptr, argPtrs.Ptrs, argCount, name))

let addIncoming (phi : ValueRef) (incoming : (ValueRef * BasicBlockRef) array) =
    let (incVals, incBlocks) = Array.unzip incoming
    use incValPtrs = new NativePtrs([|for vr in incVals -> vr.Ptr|])
    use incBlockPtrs = new NativePtrs([|for br in incBlocks -> br.Ptr|])
    let incCount = uint32 incoming.Length

    addIncomingNative (phi.Ptr, incValPtrs.Ptrs, incBlockPtrs.Ptrs, incCount)

let getNamedFunction (modRef : ModuleRef) (name : string) =
    ValueRef (getNamedFunctionNative (modRef.Ptr, name))

let optValueRef (vr : ValueRef) =
    match vr.Ptr with
    | 0n -> None
    | _ -> Some vr

let tryGetNamedFunction (modRef : ModuleRef) (name : string) =
    optValueRef (getNamedFunction modRef name)

let getParams (func : ValueRef) =
    let paramCount = int (countParams func)
    [|for i in 0 .. paramCount - 1 -> getParam func (uint32 i)|]

let getParamTypes (funcTy : TypeRef) =
    let paramCount = int (countParamTypes funcTy)
    use nativeParamTyPtrs = new NativePtrs([|for _ in 0 .. paramCount - 1 -> 0n|])
    getParamTypesNative (funcTy.Ptr, nativeParamTyPtrs.Ptrs)

    [|for ptr in nativeParamTyPtrs.PtrArr -> new TypeRef (ptr)|]

let buildSwitchWithCases
        (bldr : BuilderRef)
        (testVal : ValueRef)
        (cases : (ValueRef * BasicBlockRef) array)
        (defaultCase : BasicBlockRef) =

    let switchVal = buildSwitch bldr testVal defaultCase (uint32 cases.Length)
    for caseVal, caseBlock in cases do
        addCase switchVal caseVal caseBlock

let structTypeInContext (ctxt : ContextRef) (elemTys : TypeRef array) (packed : bool) =
    use elemTyPtrs = new NativePtrs([|for t in elemTys -> t.Ptr|])
    TypeRef (structTypeInContextNative (ctxt.Ptr, elemTyPtrs.Ptrs, uint32 elemTys.Length, packed))

let structType (elemTys : TypeRef array) (packed : bool) =
    use elemTyPtrs = new NativePtrs([|for t in elemTys -> t.Ptr|])
    TypeRef (structTypeNative (elemTyPtrs.Ptrs, uint32 elemTys.Length, packed))

let buildGEP (bldr : BuilderRef) (ptr : ValueRef) (indices : ValueRef array) (name : string) =
    use indexPtrs = new NativePtrs([|for i in indices -> i.Ptr|])
    ValueRef (buildGEPNative (bldr.Ptr, ptr.Ptr, indexPtrs.Ptrs, uint32 indices.Length, name))

let structSetBody (structTy : TypeRef) (elemTys : TypeRef array) (packed : bool) =
    use elemPtrs = new NativePtrs([|for e in elemTys -> e.Ptr|])
    structSetBodyNative(structTy.Ptr, elemPtrs.Ptrs, uint32 elemTys.Length, packed)

let getStructElementTypes (structTy : TypeRef) =
    let elemCount = int (countStructElementTypes structTy)
    use nativeElemTyPtrs = new NativePtrs([|for _ in 0 .. elemCount - 1 -> 0n|])
    getStructElementTypesNative (structTy.Ptr, nativeElemTyPtrs.Ptrs)

    [|for ptr in nativeElemTyPtrs.PtrArr -> new TypeRef (ptr)|]

let createMemoryBufferWithContentsOfFile (path : string) =
    let buffPtr = Marshal.AllocHGlobal sizeof<nativeint>
    let strPtr = Marshal.AllocHGlobal sizeof<nativeint>
    try
        // TODO mem leaks!!!
        if createMemoryBufferWithContentsOfFileNative(path, buffPtr, strPtr) then
            // TODO use strPtr message
            failwith "failed to create memory buffer"
        else
            let buff = Marshal.ReadIntPtr buffPtr
            new MemoryBufferRef(buff)
    finally
        Marshal.FreeHGlobal buffPtr
        Marshal.FreeHGlobal strPtr
