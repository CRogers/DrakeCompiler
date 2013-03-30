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

let changeSRO (text:string) = text.Replace("::", "__");

let getNumFunctionParams funcvr =
    getParams funcvr
    |> Array.length

let getInstPointTy (globals:GlobalStore) ptype =
    let nA = Map.find (match ptype with UserType s -> s) globals
    match nA.IsStruct with
        | Struct -> nA.InstanceType.Value
        | NotStruct -> nA.InstancePointerType.Value


let genClassVarGEP bldr this offset = 
    buildStructGEP bldr this (uint32 offset) ""

let genClassVarStore bldr this offset value =
    let ptr = genClassVarGEP bldr this offset
    buildStore bldr value ptr

let genClassVarLoad bldr this offset =
    let ptr = genClassVarGEP bldr this offset
    buildLoad bldr ptr ""


let getLLVMType (globals:GlobalStore) ptype =
    match ptype with
        | UserType name -> (Map.find name globals).InstanceType.Value
        | StaticType nA -> nA.StaticType.Value
        | _ -> failwithf "Can't find llvm type for %s" <| ptype.ToString()

   
let createInitStructures context (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, isStruct, ifaces, cAs) ->
            nA.InstanceType <- Some (structCreateNamed context <| changeSRO nA.QName)
            nA.StaticType <- Some (structCreateNamed context <| changeSRO nA.QName + "+Static")
            nA.VTableType <- Some (structCreateNamed context <| changeSRO nA.QName + "+VTable")
        | _ -> ()


let genClassStructure globals (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, isStruct, ifaces, cAs) ->
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
            let types = Seq.append [|nA.VTablePointerType.Value|] varTypes

            structSetBody nA.InstanceType.Value (Seq.toArray types) false

            // Static Type
            let varTypes = getLLVMVarTypes Static
            let types = Seq.append [|nA.VTablePointerType.Value|] varTypes

            structSetBody nA.StaticType.Value (Seq.toArray types) false

            // VTable Type
            structSetBody nA.VTableType.Value [|i32|] false

        | _ -> ()

let genClassProcStub globals mo (cA:ClassDeclA) =
    match cA.Item with
        | ClassVar _ -> ()
        | ClassProc (name, vis, isStatic, params_, retType, eA) ->                
            // Create a new function
            let retTy = getInstPointTy globals !retType
            let paramsTy = Seq.map (fun (p:Param) -> getInstPointTy globals p.PType) !params_ |> Seq.toArray
            let funcTy = functionType retTy paramsTy
            let func = addFunction mo (changeSRO cA.QName) funcTy

            // Set func value ref for class decl
            cA.Ref.ValueRef <- func
            cA.FuncType <- funcTy

let genClassProcStubs globals mo (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (_, _, _, _, cAs) -> Seq.iter (genClassProcStub globals mo) cAs
        | _ -> ()

let genClassStructures (globals:GlobalStore) context mo (program:seq<NamespaceDeclA>) =
    Seq.iter (createInitStructures context) program
    Seq.iter (genClassStructure globals) program
    Seq.iter (genClassProcStubs globals mo) program


let genMalloc externs bldr ty =
    let mallocFunc = Map.find "malloc" externs
    let llSize = buildIntCast bldr (sizeOf ty) i32 ""
    let mem = buildCall bldr mallocFunc [|llSize|] "malloc"
    buildBitCast bldr mem (pointerType ty 0u) ""

let genLvalue (globals:GlobalStore) bldr (eA:ExprA) =
    let getGlobal = getGlobal globals eA.Namespace eA.Usings

    let getGlobalsRef globalName refName =
        match getGlobal globalName with
            | None ->  failwith "Can't find global %s" refName
            | Some nA -> match nA.GetRef(refName) with
                | None -> failwithf "Can't find ref %s for global %s" refName globalName
                | Some r -> (nA, r)

    match eA.Item with
        | Var n ->
            // See if a ref, if not it's a
            match eA.GetRef(n) with
                | Some r -> r.ValueRef
                | None -> match getGlobal n with
                    | None -> failwith <| "Can't find global " + n
                    | Some nA -> uninitValueRef
        
        | Dot (leA, n) ->
            match leA.PType with
                | StaticType nA ->
                    match nA.GetRef(n) with
                        | Some (ClassRef cA) -> match cA.Item with
                            | ClassProc _ -> cA.Ref.ValueRef
                            | ClassVar _ -> failwithf "unimplemented loading from static vars"
                    
                | UserType typeName ->
                    let nA, ref = getGlobalsRef typeName n
                    match ref with
                        | ClassRef cA -> match cA.Item with
                            | ClassProc _ -> cA.Ref.ValueRef
                
                    

let rec genExpr (globals:GlobalStore) func bldr (eA:ExprA) =
    let genE = genExpr globals func bldr
    match eA.Item with
        | ConstUnit -> failwithf "unimplemented 98235674"
        | ConstInt (s, i) -> genConstInt (intSizeToTy s) (uint64 i)
        | ConstBool b -> genConstBool b
        | Var n ->
            match eA.GetRef(n) with
                | None -> failwithf "Can't find ref %s" n
                | Some r -> match r.RefType with
                    | LocalRef -> buildLoad bldr r.ValueRef r.Name
                    | StaticProcRef -> r.ValueRef
                    //| InstanceVarRef -> genClassVarLoad (buildLoad bldr (eA.GetRef("this").Value.ValueRef) "")
        | Dot (leA, n) ->
            // Check to see if the LHS is a StaticType, if so just return the correct thing
            match leA.PType with
                | StaticType nA -> failwithf "unimplemented 2152357238"
                | UserType typeName ->
                    let this = genE leA
                    let nA:NamespaceDeclA = Map.find typeName globals
                    match nA.GetRef(n).Value with
                        | ClassRef cA -> match cA.Item with
                            | ClassVar _ -> genClassVarLoad bldr this cA.Offset
                | _ -> failwithf "Can only perform dot operation on static or instance type"

        | Binop _ ->
            failwithf "Binop has not been lowered to Call at codegen time - compiler fail"

        | Call (eA, args) ->
            let argEs = List.map genE args

            let funcvr, fixedArgs = match eA.Item with 
                | Var n ->
                    let ref = eA.GetRef(n).Value
                    let funcvr = match ref.RefType with
                        | InstanceProcRef | StaticProcRef -> ref.ValueRef
                        | _ -> failwith "Can only call a method"

                    let fixedArgs = match ref.RefType with
                        | StaticProcRef -> argEs
                        | InstanceProcRef ->
                            let thisLoc = eA.GetRef("this").Value.ValueRef
                            buildLoad bldr thisLoc "this" :: argEs

                    (funcvr, fixedArgs)

                | Dot (leA, n) ->
                    let getProcVR (nA:NamespaceDeclA) = match nA.GetRef(n).Value with
                        | ClassRef cA -> match cA.Item with
                            | ClassProc _ -> (cA.Ref.ValueRef, cA.IsStatic)
                            | _ -> failwith "Can only call methods"
                        | _ -> failwithf "unimplemented 11746238462194"

                    match leA.PType with
                        | StaticType nA -> 
                            let funcvr, _ = getProcVR nA
                            (funcvr, argEs)

                        | UserType typeName ->
                            let nA = Map.find typeName globals
                            let funcvr, isStatic = getProcVR nA
                            let fixedArgs = match isStatic with
                                | Static -> argEs
                                | NotStatic ->
                                    // Put this on the front
                                    genE leA :: argEs
                            (funcvr, fixedArgs)

            buildCall bldr funcvr (Array.ofList fixedArgs) ""

        | DeclVar (name, assignA) ->
            genE assignA
        | Assign (lvalue, eA) ->
            let addr = genLvalue globals bldr lvalue
            let e = genE eA
            buildStore bldr e addr
        | Return eA ->
            buildRet bldr <| genE eA
        | ReturnVoid ->
            buildRetVoid bldr
        | If (test, then_, else_) ->
            let thenRet = isLastInSeqRet then_
            let elseRet = isLastInSeqRet else_

            let ifthen = appendBasicBlock func "ifthen"
            let ifelse = appendBasicBlock func "ifelse"
            let ifcont = appendBasicBlock func "ifcont"
            // If
            let testexpr = genE test
            buildCondBr bldr testexpr ifthen ifelse |> ignore
            // Then
            positionBuilderAtEnd bldr ifthen
            genE then_ |> ignore
            if not thenRet then buildBr bldr ifcont |> ignore
            // Else
            positionBuilderAtEnd bldr ifelse
            genE else_ |> ignore
            if not elseRet then buildBr bldr ifcont |> ignore
            // Cont
            if thenRet && elseRet then
                removeBasicBlockFromParent ifcont
            else
                positionBuilderAtEnd bldr ifcont
            uninitValueRef
        | Seq (eA1, eA2) ->
            genE !eA1 |> ignore
            genE !eA2
        | Nop ->
            uninitValueRef

let genClass (globals:GlobalStore) mo (cA:ClassDeclA) =
    match cA.Item with
        | ClassVar _ -> ()
        | ClassProc (name, vis, isStatic, params_, retType, eA) ->
            use bldr = new Builder()
            let func = cA.Ref.ValueRef

            // Basic block
            let entry = appendBasicBlock func "entry"
            positionBuilderAtEnd bldr entry

            // Allocate space for local variables
            Seq.iter (fun (r:Ref) -> r.ValueRef <- buildAlloca bldr (getInstPointTy globals r.PType) r.Name) eA.LocalVars

            let paramsTy = getParamTypes cA.FuncType

            // Get the value refs for the function params and set the function expr's ref's valuerefs
            Seq.iteri (fun i (p:Param) ->
                let llvmParam = getParam func <| uint32 i
                setValueName llvmParam p.Name
                let stackSpace = buildAlloca bldr paramsTy.[i] p.Name
                buildStore bldr llvmParam stackSpace |> ignore
                (eA.GetRef(p.Name) |> Option.get).ValueRef <- stackSpace) !params_

            // Execute procedure body
            let expr = genExpr globals func bldr eA

            ()


let genNamespace globals externs mo (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, isStruct, ifaces, cAs) ->
            // Make object allocation ctor func
            let ctorFuncTy = functionType nA.InstancePointerType.Value [||]
            let ctorFunc = addFunction mo (changeSRO nA.QName + "+ctor") ctorFuncTy

            let entry = appendBasicBlock ctorFunc "entry"
            use bldr = new Builder();
            positionBuilderAtEnd bldr entry

            // Body for ctor func
            let this = genMalloc externs bldr nA.InstanceType.Value
            
            let initClassVars (cA:ClassDeclA) = match cA.Item with
                | ClassVar (name, vis, NotStatic, ptype, eA) ->
                    let expr = genExpr globals ctorFunc bldr eA
                    genClassVarStore bldr this cA.Offset expr |> ignore
                | _ -> ()

            // Initialise instance variables
            Seq.iter initClassVars cAs

            // Return the this pointer
            buildRet bldr this |> ignore

            // Set the ref for the ctor to the built function
            nA.CtorRef.ValueRef <- ctorFunc

            // TODO: Static class vars

            // Gen the code for the procedures
            Seq.iter (genClass globals mo) cAs
        | Interface (name, vis, ifaces, iAs) -> ()//Seq.iter genInterface iAs


let genExterns mo =
    let addExtern name funcTy = 
        let func = addFunction mo name funcTy
        addFunctionAttr func Attribute.NoUnwindAttribute
        (name, func)

    let addExternC name retTy argTys =
        addExtern name <| functionType retTy argTys

    let addStrConst name (str:string) =
        let len = uint32 (str.Length)
        // Add one to length for null terminator
        let globTy = arrayType i8 (len + 1u)
        let glob = addGlobal mo globTy name
        setLinkage glob Linkage.InternalLinkage
        setGlobalConstant glob true
        setInitializer glob (constString str len false)
        (name, glob)
        

    [
        addExternC  "malloc" i8p [|i32|];
        addExternC  "puts" i32 [|i8p|];
        addExtern   "printf" <| varArgFunctionType i32 [|i8p|];
        addStrConst "numFmt" "%d\n"
    ]
    |> Map.ofSeq
   

let genMain mo (program:seq<NamespaceDeclA>) =
    // Make main entry function
    let main = addFunction mo "main" <| functionType i32 [||]
    let entry = appendBasicBlock main "entry"
    use bldr = new Builder()
    positionBuilderAtEnd bldr entry
    
    // Find the class with the static main() method and run it first
    let cAs = getClassDecls program
    let cAmain = Seq.tryFind (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc ("main", Public, Static, params_, _, _ ) when !params_ = [] -> true
        | _ -> false) cAs

    match cAmain with
        | None -> failwithf "No public static main() method found"
        | Some cA ->
            buildCall bldr cA.Ref.ValueRef [||] "" |> ignore

    // Add terminator
    buildRet bldr (genConstInt i32 0UL) |> ignore
            

let gen (globals:GlobalStore) (program:list<NamespaceDeclA>) =
    let context = contextCreate ()
    let mo = moduleCreateWithNameInContext (DateTime.Now.ToLongDateString()) context

    // Define some important externs
    let externs = genExterns mo

    // Separat classes into builtins and not builtins
    let all = List.map (fun (nA:NamespaceDeclA) -> Util.either nA.IsBuiltin nA) program
    let builtins = Util.lefts all
    let nonBuiltins = Util.rights all

    // Build the builtin stuff    
    Builtins.builtinSetUpTypes globals
    Seq.iter (genClassProcStubs globals mo) builtins
    Builtins.builtinGenInts globals
    Builtins.builtinGenBool globals
    Builtins.builtinGenConsole externs globals

    // Build the structures required to store the information
    genClassStructures globals context mo nonBuiltins

    // Build the class/interface operations themselves
    Seq.iter (genNamespace globals externs mo) nonBuiltins

    // Generate the initial main function
    genMain mo program
    
    mo