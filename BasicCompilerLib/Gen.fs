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


let gen (globals:GlobalStore) (program:seq<NamespaceDeclA>) =
    let context = contextCreate ()
    let mo = moduleCreateWithNameInContext (DateTime.Now.ToLongDateString()) context
    genClassStructures globals context program
    mo