module Gen

open System.Collections.Generic
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let i1 = int1Type ()
let i8 = int8Type ()
let i16 = int16Type ()
let i32 = int32Type ()
let i64 = int64Type ()
let tyVoid = voidType ()
let i32zero = constInt i32 0UL false
let i8p = pointerType (int8Type ()) 0u

let globalFuncs = new Dictionary<string, Func>()

let globals = new Dictionary<string, ValueRef>()
let addGlobalExtern mo name funcTy =
    let func = addFunction mo name funcTy
    addFunctionAttr func Attribute.NoUnwindAttribute
    globals.Add(name, func)
    func

let addGlobalStringConstant mo name (str:string) =
    let len = uint32 (str.Length)
    // Add one to length for null terminator
    let globTy = arrayType i8 (len + 1u)
    let glob = addGlobal mo globTy name
    setLinkage glob Linkage.InternalLinkage
    setGlobalConstant glob true
    setInitializer glob (constString str len false)
    globals.Add(name, glob)
    glob



let mkConst x = constInt i32 (uint64 x) false

let boolToI1 x = constInt i1 (if x then 1UL else 0UL) false

let typeToLLVMType t = match t with
    | Unit -> i32
    | Bool -> i1
    | Int 8  -> i8
    | Int 16 -> i16
    | Int 32 -> i32
    | Int 64 -> i64

let rec genExpr bldr (env:Environ) (exprA:ExprA) =
    match exprA.Item with
        | ConstInt x ->
            mkConst x
        | ConstBool b ->
            boolToI1 b
        | ConstUnit ->
            mkConst 0
        | Binop (op, left, right) ->
            let bIcmp cond = (fun bldr -> buildICmp bldr cond)
            let buildFunc = match op with
                | Add ->        buildAdd
                | Sub ->        buildSub
                | Div ->        buildSDiv
                | Mul ->        buildMul
                | BoolAnd ->    buildAnd
                | BoolOr ->     buildOr
                | Lt ->         bIcmp IntPredicate.IntSLT 
                | Gt ->         bIcmp IntPredicate.IntSGT 
                | LtEq ->       bIcmp IntPredicate.IntSLE 
                | GtEq ->       bIcmp IntPredicate.IntSGE 
                | Eq ->         bIcmp IntPredicate.IntEQ
            buildFunc bldr (genExpr bldr env left) (genExpr bldr env right) ""
            
        | Call (name, args) ->
            let funcRef = exprA.GetRef(name)
            let argRefs = Seq.map (genExpr bldr env) args |> Seq.toArray
            buildCall bldr funcRef.ValueRef argRefs ""

        | Var name ->
            let ref = exprA.GetRef(name)
            match ref.RefType with
                | Parameter -> ref.ValueRef
                | Local     -> buildLoad bldr ref.ValueRef name

        | Assign (name, e) ->
            let expr = genExpr bldr env e
            let valref = exprA.GetRef(name).ValueRef
            buildStore bldr expr valref
            // Return the value that was stored rather than the store itself
            expr

        | Print e -> 
            let expr = genExpr bldr env e
            let mexpr = if e.PType = Bool then buildZExt bldr expr i32 "convert." else expr
            let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] ""
            buildCall bldr globals.["printf"] [| gep; mexpr |] <| ""

        | DeclVar (name, assignA) ->
            genExpr bldr env assignA

        | Return e ->
            let expr = genExpr bldr env e
            buildRet bldr expr

        | If (e, thenStmtAs, elseStmtAs) ->
            let func = env.EnclosingFunc.ValueRef
            let ifthen = appendBasicBlock func "ifthen"
            let ifelse = appendBasicBlock func "ifelse"
            let ifcont = appendBasicBlock func "ifcont"
            // If
            let expr = genExpr bldr env e
            buildCondBr bldr expr ifthen ifelse |> ignore
            // Then
            positionBuilderAtEnd bldr ifthen
            let thenResult = genExpr bldr env thenStmtAs
            buildBr bldr ifcont |> ignore
            // Else
            positionBuilderAtEnd bldr ifelse
            let elseResult = genExpr bldr env elseStmtAs
            buildBr bldr ifcont |> ignore
            // Cont
            positionBuilderAtEnd bldr ifcont
            mkConst 0

        | While (e, stmtAs) ->
            let func = env.EnclosingFunc.ValueRef
            let whilebody = appendBasicBlock func "whilebody"
            let whiletest = appendBasicBlock func "whiletest"
            let whilecont = appendBasicBlock func "whilecont"
            // Jump to test
            buildBr bldr whiletest |> ignore
            // Body
            positionBuilderAtEnd bldr whilebody
            let res = genExpr bldr env stmtAs
            buildBr bldr whiletest |> ignore
            // Test
            positionBuilderAtEnd bldr whiletest
            let expr = genExpr bldr env e
            buildCondBr bldr expr whilebody whilecont |> ignore
            // Cont
            positionBuilderAtEnd bldr whilecont
            res

        | Seq (e1A, e2A) ->
            // Generate the code for the first expr, throw away the result
            genExpr bldr env e1A
            // Gen code for next expr
            genExpr bldr env e2A

let genDecl module_ (declA:DeclA) = match declA.Item with
    | Proc (name, _, _, body) ->
        let func = declA.GetRef(name)
        let env = Environ(module_, func)
        use bldr = new Builder()
        positionBuilderAtEnd bldr (appendBasicBlock func.ValueRef "entry")
        // Allocate local vars on stack
        Seq.iter (fun (var:Ref) -> var.ValueRef <- buildAlloca bldr (typeToLLVMType var.PType) var.Name) declA.LocalVars
        // Build body
        genExpr bldr env body |> ignore

let defineDecl myModule (declA:DeclA) = match declA.Item with
    | Proc (name, params, returnType, _) ->
        let numParams = params.Length
        let argTy = Seq.toArray <| Seq.map (fun (t:Param) -> typeToLLVMType t.PType) params
        let returnTy = typeToLLVMType returnType
        let funcTy = functionType returnTy argTy
        let func = addFunction myModule name funcTy

        let foo = [0..numParams-1]

        // Set the function parameter names and fill the map
        Seq.iter (fun i -> 
            let paramName = params.[i].Name
            let llvmParam = getParam func <| uint32 i
            declA.GetRef(paramName).ValueRef <- llvmParam
            // Set the function param name and return a tuple for the map
            setValueName llvmParam |> ignore) [0..numParams-1]

        (name, func)

let gen (program:list<DeclA>) =
    let myModule = moduleCreateWithName "basicModule"
    
    let printfTy = varArgFunctionType i32 [| i8p |]
    let printf = addGlobalExtern myModule "printf" printfTy

    // Add %d numFmt
    addGlobalStringConstant myModule "numFmt" "%d\n" |> ignore

    // Make funcTypes for each procedure
    Seq.iter (fun declA -> 
        let (name, func) =defineDecl myModule declA
        declA.GetRef(name).ValueRef <- func) program

    // Generate code for each proc
    Seq.iter (genDecl myModule) program

    myModule