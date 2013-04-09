module GenStructs

open Tree
open LLVMTypes
open LLVM.Core
open LLVM.Generated.Core
open LLVMFSExtensions
open GenUtil


let numberInterfaceProcs (nAs:seq<NDA>) =
    let mutable globalOffset = 0

    // For each item, if it is an interface, give an offset to each of it's InterfaceProcs
    for iA in getInterfaceDecls nAs do
        iA.GlobalOffset <- globalOffset
        globalOffset <- globalOffset + 1

    globalOffset

let createInitVTableType context = structCreateNamed context "VTable"

let createInterfaceType context vtableType = 
    let ifaceTy = structCreateNamed context "Interface"
    structSetBody ifaceTy [| pointerType vtableType 0u |] false
    ifaceTy

let createInitStructures context vtableType ifaceType (nA:NDA) =
    match nA.Item with
        | Class (name, vis, isStruct, ifaces, cAs) ->
            nA.InstanceType <- Some (structCreateNamed context <| changeSRO nA.QName)
            nA.StaticType <- Some (structCreateNamed context <| changeSRO nA.QName + "+Static")
            nA.VTableType <- Some vtableType            
        | Interface (name, vis, ifaces, iAs) ->
            // Set the instance type of an interface to be a struct with a pointer to a VTable
            nA.InstanceType <- Some ifaceType


let createNamespaceStructures (nA:NDA) = match nA.Item with
    | Class (name, vis, isStruct, ifaces, cAs) ->
        let getLLVMVarTypes isStatic =
            let varTypesOptions =
                Seq.mapi (fun i (cA:CDA) ->
                    match cA.Item with
                        | ClassVar (name, vis, iS, ptype, eA) when iS = isStatic ->
                            // i+1 as we have to include the vtable pointer/ptr to class name
                            cA.Offset <- i+1
                            Some <| getLLVMType !ptype
                        | _ -> None) cAs
                    
            // Get rid of the Nones
            Util.getSomes varTypesOptions

        // Instance Type
        let varTypes = getLLVMVarTypes NotStatic
        let types = Seq.append [|nA.VTablePointerType.Value|] varTypes

        structSetBody nA.InstanceType.Value (Seq.toArray types) false

        // Static Type
        let varTypes = getLLVMVarTypes Static
        let types = Seq.append [|nA.VTablePointerType.Value|] varTypes

        structSetBody nA.StaticType.Value (Seq.toArray types) false

    | _ -> ()


let createClassProcStub mo (cA:CDA) =
    match cA.Item with
        | ClassVar _ -> ()
        | ClassProc (name, vis, isStatic, params_, retType, eA) ->                
            // Create a new function
            let retTy = getInstPointTy !retType
            let paramsTy = Seq.map (fun (p:Param) -> getInstPointTy p.PType) !params_ |> Seq.toArray
            let funcTy = functionType retTy paramsTy
            let func = addFunction mo (changeSRO cA.QName) funcTy

            // Set func value ref for class decl
            cA.Ref.ValueRef <- Some func
            cA.FuncType <- Some funcTy

let createClassProcStubs mo nAs =
    Seq.iter (createClassProcStub mo) <| getClassDecls nAs

let createVTableType vtableType ifaceType (nAs:list<NDA>) =
    // Mapping from vtable offset to function type
    let offsetTy = List.ofSeq <| seq {
        for iA in getInterfaceDecls nAs do
            let argTys = List.toArray ((pointerType ifaceType 0u) :: [ for p in iA.Params -> getInstPointTy p.PType ])
            let funcTy = functionType (getInstPointTy iA.PType) argTys
            iA.FuncType <- Some funcTy
            yield (iA.GlobalOffset, pointerType funcTy 0u)
    }

    let tyArr = 
        offsetTy
        |> List.sortBy fst
        |> List.map snd
        |> List.toArray
    
    structSetBody vtableType tyArr false


// Make stub function that takes an Interface* as it's this and casts to the correct User::Type* and calls the correct function
let createIfaceProcStub mo ifaceType (cA:CDA) = match cA.Item with
    | ClassProc (_, _, _, params_, retType, _) ->
        let retTy = getInstPointTy !retType
        // We need to change the this param to be of Interface type rather than it's concrete type
        let argTys = (pointerType ifaceType 0u) :: [ for p in List.tail !params_ -> getInstPointTy p.PType ]
        let funcTy = functionType retTy <| List.toArray argTys

        let func = addFunction mo (cA.QName + "+Stub") funcTy
        use bldr = new Builder()
        let entry = appendBasicBlock func "entry"
        positionBuilderAtEnd bldr entry

        let firstParam = getParam func 0u
        let otherParams = [ for i in [1..((!params_).Length-1)] -> getParam func (uint32 i) ]

        // Add names to params
        setValueName firstParam "this"
        Seq.iter2 (fun (p:Param) llvmp -> setValueName llvmp p.Name) (List.tail !params_) otherParams

        let destTy = getInstPointTy (List.head !params_).PType
        let firstParamCast = buildBitCast bldr firstParam destTy ""

        //let firstParamCastInt = buildIntCast bldr firstParam i32 ""
        //let firstParamCast = buildPointerCast bldr firstParamCastInt (getInstPointTy (List.head !params_).PType) ""
        let allParams = List.toArray (firstParamCast :: otherParams)

        buildCall bldr cA.Ref.ValueRef.Value allParams ""
        |> buildRet bldr
        |> ignore

        func


let createVTable mo ifaceType numIProcs (nA:NDA) = match nA.Item with
    | Class (_, _, _, _, cAs) ->
        let vtableType = nA.VTableType.Value

        // Initialise the vtable with null pointers to functions
        let arr = [| for ty in getStructElementTypes vtableType -> constPointerNull ty |]

        // Add the functions appropriate for each implemented method
        for cA in cAs do
            match cA.DefiningMethod with
                | None -> ()
                | Some iA ->
                    let stub = createIfaceProcStub mo ifaceType cA
                    cA.IfaceProcStub <- Some stub
                    arr.[iA.GlobalOffset] <- stub

        // Create the constant vtable
        let cs = constNamedStruct vtableType arr
        // Add a global
        let g = addGlobal mo vtableType <| "VTable___" + changeSRO nA.QName
        setInitializer g cs
        let gep = constGEP g [|genConstInt i32 0UL|]
        //let casted = constBitCast gep (pointerType vtableType 0u)
        nA.VTable <- Some gep
    | _ -> ()


let genStructs context mo (program:list<NDA>) =
    let numIProcs = numberInterfaceProcs program
    let vtableType = createInitVTableType context
    let ifaceType = createInterfaceType context vtableType

    Seq.iter (createInitStructures context vtableType ifaceType) program
    Seq.iter createNamespaceStructures program

    createClassProcStubs mo program

    createVTableType vtableType ifaceType program
    Seq.iter (createVTable mo ifaceType numIProcs) program

