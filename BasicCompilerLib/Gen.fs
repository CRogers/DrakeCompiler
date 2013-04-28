module Gen

open System
open LLVMTypes
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter
open GenUtil
open GenStructs

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

let genMalloc externs bldr (nA:NDA) =
    let mallocFunc = Map.find "malloc" externs
    let llSize = buildIntCast bldr (sizeOf nA.InstanceType.Value) i32 ""
    let mem = buildCall bldr mallocFunc [|llSize|] "malloc"
    // Set the VTable pointer
    let obj = buildBitCast bldr mem (nA.InstancePointerType.Value) ""
    genClassVarStore bldr obj 0 nA.VTable.Value |> ignore
    obj

let rec genExpr pIfaceTy func bldr (eA:ExprA) =
    let genE = genExpr pIfaceTy func bldr
    
    let genLValue (lvalue:ExprA) =
        match lvalue.Item with
            | Var n -> match lvalue.GetRef(n) with
                | Some ref -> match ref.RefType with
                    | LocalRef -> ref.ValueRef.Value
                    | InstanceVarRef cA -> genClassVarGEP bldr (buildLoad bldr (lvalue.GetRef("this").Value.ValueRef.Value) "") cA.Offset
            | DotStatic (nA, cA) ->
                failwithf "unimplemented 347853"
            | DotInstance (dotEA, cA) ->
                let dotE = genExpr pIfaceTy func bldr dotEA
                genClassVarGEP bldr dotE cA.Offset

    let genArgsWithCasts eAs params_ =
        Seq.map2 (fun eA (p:Param) -> match (ptypeToNA p.PType).Item with
            | Class _ -> genE eA
            | Interface _ -> buildBitCast bldr (genE eA) pIfaceTy "") eAs params_
        |> List.ofSeq

    match eA.Item with
        | ConstInt (s, i) -> genConstInt (intSizeToTy s) (uint64 i)
        | ConstBool b -> genConstBool b
        | Var n ->
            match eA.GetRef(n) with
                | None -> failwithf "Can't find ref %s" n
                | Some r -> match r.RefType with
                    | LocalRef -> buildLoad bldr r.ValueRef.Value r.Name
                    | StaticProcRef -> r.ValueRef.Value
                    | InstanceVarRef cA -> genClassVarLoad bldr (buildLoad bldr (eA.GetRef("this").Value.ValueRef.Value) "") cA.Offset

        | VarStatic _   -> failwithf "Compiler fail: This VarStatic should have been lowered to a more specific Dot in the annotation stage"
        | Dot _         -> failwithf "Compiler fail: This Dot should have been lowered to a more specific Dot in the annotation stage"
        | DotTemplate _ -> failwithf "Compiler fail: This DotTemplate should have been lowered to a more specific Dot in the annotation stage"
        | Binop _       -> failwithf "Compiler fail: This Binop should have been lowered to a CallStatic in the annotation stage"
        | Call _        -> failwithf "Compiler fail: This Call should have been lowered to a more specific Call in the annotation stage"

        | DotStatic (nA, cA) ->
            // Static var
            failwithf "unimplemented 452784"

        | DotInstance (eA, cA) ->
            let e = genE eA
            genClassVarLoad bldr e cA.Offset

        | Cast (ptype, eA) ->
            buildBitCast bldr (genE eA) pIfaceTy ""

        | CallStatic (cA, args) ->
            let argEs = genArgsWithCasts args cA.Params |> Array.ofSeq
            let funcvr = cA.Ref.ValueRef.Value
            buildCall bldr funcvr argEs ""

        | CallInstance (cA, feA, args) ->
            let argEs = genArgsWithCasts args cA.Params
            let thisE = genE feA
            let fixedArgs = List.toArray (thisE :: argEs)

            let funcvr = cA.Ref.ValueRef.Value
            buildCall bldr funcvr fixedArgs ""

        | CallVirtual (iA, feA, args) ->
            let argEs = genArgsWithCasts args iA.Params
            let this = genE feA
            // We must cast this to an Interface* for the VTable functions
            let casted = buildBitCast bldr this pIfaceTy ""

            // Get concrete stub out of the vtable struct
            let vtableGEP = buildStructGEP bldr casted 0u ""
            let vtable = buildLoad bldr vtableGEP ""
            let funcGEP = buildStructGEP bldr vtable 0u ""
            let func = buildLoad bldr funcGEP ""
            
            // Call vfunc
            let fixedArgs = List.toArray (casted :: argEs)
            buildCall bldr func fixedArgs ""

        | DeclVar (name, assignA) ->
            genE assignA

        | Assign (lvalue, right) ->
            let addr = genLValue lvalue

            let e = genE right
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

        | While _ -> failwithf "Unimplemented while loops"

        | Seq (eA1, eA2) ->
            genE !eA1 |> ignore
            genE !eA2

        | Nop ->
            uninitValueRef

let genClass mo pIfaceTy (cA:ClassDeclA) =
    if not cA.IsCtor then match cA.Item with
        | ClassVar _ -> ()
        | ClassProc (name, vis, isStatic, params_, retType, eA) ->
            use bldr = new Builder()
            let func = cA.Ref.ValueRef.Value

            // Basic block
            let entry = appendBasicBlock func "entry"
            positionBuilderAtEnd bldr entry

            // Allocate space for local variables
            Seq.iter (fun (r:Ref) -> r.ValueRef <- Some <| buildAlloca bldr (getInstPointTy r.PType) r.Name) eA.LocalVars

            let paramsTy = getParamTypes cA.FuncType.Value

            // Get the value refs for the function params and set the function expr's ref's valuerefs
            Seq.iteri (fun i (p:Param) ->
                let llvmParam = getParam func <| uint32 i
                setValueName llvmParam p.Name
                let stackSpace = buildAlloca bldr paramsTy.[i] p.Name
                buildStore bldr llvmParam stackSpace |> ignore
                (eA.GetRef(p.Name) |> Option.get).ValueRef <- Some stackSpace) !params_

            // Execute procedure body
            let expr = genExpr pIfaceTy func bldr eA

            ()


let genNamespace externs globals mo pIfaceTy (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, isStruct, ifaces, _) ->
            let cAs = nA.ClassDeclAs

            // Make object allocation ctor func
            let ctorFuncTy = functionType nA.InstancePointerType.Value [||]
            let ctorFunc = addFunction mo (changeSRO nA.QName + "+ctor") ctorFuncTy

            let entry = appendBasicBlock ctorFunc "entry"
            use bldr = new Builder();
            positionBuilderAtEnd bldr entry

            // Body for ctor func
            let this = genMalloc externs bldr nA
            
            let initClassVars (cA:ClassDeclA) = match cA.Item with
                | ClassVar (name, vis, NotStatic, ptype, eA) ->
                    if not (eA.PType = commonPtype globals Unit) then
                        let expr = genExpr pIfaceTy ctorFunc bldr eA
                        genClassVarStore bldr this cA.Offset expr |> ignore
                | _ -> ()

            // Initialise instance variables
            Seq.iter initClassVars cAs

            // Return the this pointer
            buildRet bldr this |> ignore

            // Set the ref for the ctor to the built function
            nA.CtorCA.Ref.ValueRef <- Some ctorFunc

            // TODO: Static class vars

            // Gen the code for the procedures
            Seq.iter (genClass mo pIfaceTy) cAs
        | Interface (name, vis, ifaces, _) -> ()//Seq.iter genInterface iAs


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
    let cAs = getCIClassDecls program
    let cAmain = Seq.tryFind (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc ("main", Public, Static, params_, _, _ ) when !params_ = [] -> true
        | _ -> false) cAs

    match cAmain with
        | None -> failwithf "No public static main() method found"
        | Some cA ->
            buildCall bldr cA.Ref.ValueRef.Value [||] "" |> ignore

    // Add terminator
    buildRet bldr (genConstInt i32 0UL) |> ignore
            

let gen (globals:GlobalStore) =
    let program = globalsToNAs globals

    let context = contextCreate ()
    let mo = moduleCreateWithNameInContext (DateTime.Now.ToLongDateString()) context

    // Define some important externs
    let externs = genExterns mo

    // Separate classes into builtins and not builtins
    let all = Seq.map (fun (nA:NamespaceDeclA) -> Util.either nA.IsBuiltin nA) program
    let builtins = Util.lefts all
    let nonBuiltins = List.ofSeq <| Util.rights all

    // Build the builtin stuff    
    Builtins.builtinSetUpTypes globals
    createClassProcStubs mo builtins
    Builtins.builtinGenInts globals
    Builtins.builtinGenBool globals
    Builtins.builtinGenConsole externs globals

    // Build the structures required to store the information
    let pIfaceTy = genStructs globals context mo nonBuiltins

    // Build the class/interface operations themselves
    Seq.iter (genNamespace externs globals mo pIfaceTy) nonBuiltins

    // Generate the initial main function
    genMain mo program
    
    mo