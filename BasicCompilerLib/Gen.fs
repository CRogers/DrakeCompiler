module Gen

open System
open LLVMTypes
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter


(*

+-----------------+             +-------------------+        +-----------------+
| Vtable Pointer  +-------------> Ptr to class name +-------->Class Name String|
|-----------------|             |-------------------|        +-----------------+
|        .        |             |         .         |
|        .        |             |         .         |
|        .        |             |         .         |
|   Class Vars    |             |      Vtable       |
|        .        |             |         .         |
|        .        |             |         .         |
|        .        |             |         .         |
+-----------------+             +-------------------+

*)

let getLLVMType (globals:GlobalStore) ptype =
    match ptype with
        | UserType name -> (Map.find name globals).InstanceType
        | StaticType name -> (Map.find name globals).StaticType
        | _ -> failwithf "Can't find llvm type for %s" <| ptype.ToString()

let genClassStructures (globals:GlobalStore) context (program:seq<NamespaceDeclA>) =
    
    let createInitStructures (nA:NamespaceDeclA) =
        match nA.Item with
            | Class (name, vis, cAs) ->
                nA.InstanceType <- structCreateNamed context nA.QName
                nA.StaticType <- structCreateNamed context <| nA.QName + "+Static"
                nA.VTableType <- structCreateNamed context <| nA.QName + "+VTable"
            | _ -> ()


    let genClassStructure (nA:NamespaceDeclA) =
        match nA.Item with
            | Class (name, vis, cAs) ->
                let getLLVMVarTypes isStatic =
                    let varTypesOptions =
                        Seq.mapi (fun i (cA:ClassDeclA) ->
                            match cA.Item with
                                | ClassVar (name, vis, iS, ptype, eA) when iS = isStatic ->
                                    // i+1 as we have to include the vtable pointer/ptr to class name
                                    cA.Offset <- i+1
                                    Some <| getLLVMType globals !ptype
                                | _ -> None) cAs
                    
                    // Get rid of the Nones
                    Seq.filter Option.isSome varTypesOptions |> Seq.map Option.get

                // Instance Type
                let varTypes = getLLVMVarTypes NotStatic
                let types = Seq.append [|nA.VTablePointerType|] varTypes

                structSetBody nA.InstanceType (Seq.toArray types) false

                // Static Type
                let varTypes = getLLVMVarTypes Static
                let types = Seq.append [|nA.VTablePointerType|] varTypes

                structSetBody nA.StaticType (Seq.toArray types) false

                // VTable Type
                structSetBody nA.VTableType [|i32|] false

            | _ -> ()

    
    Seq.iter createInitStructures program
    Seq.iter genClassStructure program


let genConstInt ty x = constInt ty (uint64 x) false

let genMalloc externs bldr ty =
    let mallocFunc = Map.find "malloc" externs
    let llSize = buildIntCast bldr (sizeOf ty) i32 ""
    let mem = buildCall bldr mallocFunc [|llSize|] "malloc"
    buildBitCast bldr mem (pointerType ty 0u) ""


let genExpr bldr (eA:ExprA) =
    match eA.Item with
        | _ -> genConstInt i32 99    


let genClass (globals:GlobalStore) mo (cA:ClassDeclA) =    
    match cA.Item with
        | ClassVar _ -> ()
        | ClassProc (name, vis, Static, params_, retType, eA) ->  
            use bldr = new Builder()
            // Create a new function
            let getIPT ptype = (Map.find (match !retType with UserType s -> s) globals).InstancePointerType
            let retTy = getIPT retType
            let paramsTy = Seq.map (fun (p:Param) -> getIPT p.PType) params_ |> Seq.toArray
            let funcTy = functionType retTy paramsTy
            let func = addFunction mo cA.QName funcTy

            // Basic block
            let entry = appendBasicBlock func "entry"
            positionBuilderAtEnd bldr entry
            
            // Execute procedure body
            let expr = genExpr bldr eA
            ()
        | ClassProc (name, vis, NotStatic, params_, retType, eA) -> ()


let genClassVarStore bldr this offset value =
    let ptr = buildStructGEP bldr this (uint32 offset) ""
    buildStore bldr value ptr

let genNamespace globals externs mo (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, cAs) ->
            // Make object allocation ctor func
            let ctorFuncTy = functionType nA.InstancePointerType [||]
            let ctorFunc = addFunction mo (nA.QName + "+ctor") ctorFuncTy

            let entry = appendBasicBlock ctorFunc "entry"
            use bldr = new Builder();
            positionBuilderAtEnd bldr entry

            // Body for ctor func
            let this = genMalloc externs bldr nA.InstanceType
            
            let initClassVars (cA:ClassDeclA) = match cA.Item with
                | ClassVar (name, vis, NotStatic, ptype, eA) ->
                    let expr = genExpr bldr eA
                    Some <| genClassVarStore bldr this cA.Offset expr
                | _ -> None

            let varInits = Seq.map initClassVars cAs |> Util.getSomes
            printfn "%d" <| Seq.length varInits

            // Return the this pointer
            buildRet bldr this |> ignore

            // Set the ref for the ctor to the built function
            nA.CtorRef.ValueRef <- ctorFunc

            // TODO: Static class vars

            // Gen the code for the procedures
            Seq.iter (genClass globals mo) cAs
        | Interface (name, vis, iAs) -> ()//Seq.iter genInterface iAs


let genExterns mo =
    let addExtern name retTy argTys =
        let funcTy = functionType retTy argTys
        let func = addFunction mo name funcTy
        addFunctionAttr func Attribute.NoUnwindAttribute
        (name, func)

    [
        addExtern "malloc" i8p [|i32|];
        addExtern "puts" i32 [|i8p|]
    ]
    |> Map.ofSeq
    

let gen (globals:GlobalStore) (program:seq<NamespaceDeclA>) =
    let context = contextCreate ()
    let mo = moduleCreateWithNameInContext (DateTime.Now.ToLongDateString()) context

    // Define some important externs
    let externs = genExterns mo

    // Build the structures required to store the information
    genClassStructures globals context program

    // Build the class/interface operations themselves
    Seq.iter (genNamespace globals externs mo) program
    
    mo