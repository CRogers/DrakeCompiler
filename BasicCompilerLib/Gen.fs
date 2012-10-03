module Gen

open System.Collections.Generic
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let i32 = int32Type ()
let tyVoid = voidType ()
let i8 = int8Type ()
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

let rec genExpr bldr (env:Environ) irname (exprA:ExprA) =
    let llvmname = if irname.Equals("") then env.GetTmp() else env.GetName irname
    match exprA.Expr with
        | ConstInt x ->
            mkConst x
        | Binop (op, left, right) ->
            let buildFunc = match op with
                | Add -> buildAdd
                | Sub -> buildSub
                | Div -> buildSDiv
                | Mul -> buildMul
            buildFunc bldr (genExpr bldr env "" left) (genExpr bldr env "" right) llvmname
        | Call (name, args) ->
            let funcToCall = globalFuncs.[name]
            let argRefs = Seq.map (genExpr bldr env "") args |> Seq.toArray
            buildCall bldr funcToCall.Func argRefs llvmname
        | Var name ->
            env.GetRef(name)

let genStmt bldr env (stmtA:StmtA) = match stmtA.Stmt with
    | Print e -> 
        let expr = genExpr bldr env "print" e
        let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] (env.GetTmp())
        buildCall bldr globals.["printf"] [| gep; expr |] <| env.GetTmp()
    | Assign (name, e) ->
        let expr = genExpr bldr env name e
        env.AddRef(name, expr)
        expr
    | Return e ->
        let expr = genExpr bldr env "return" e
        buildRet bldr expr


let genDecl (declA:DeclA) = match declA.Decl with
    | Proc (name, _, _, stmts) ->
        let func = globalFuncs.[name]
        let env = Environ(func)
        use bldr = new Builder()
        positionBuilderAtEnd bldr (appendBasicBlock func.Func "entry")
        Seq.iter (fun s -> genStmt bldr env s |> ignore) stmts

let defineDecl myModule (declA:DeclA) = match declA.Decl with
    | Proc (name, params, returnType, _) ->
        let numParams = params.Length
        let argTy = Array.create numParams i32
        let funcTy = functionType i32 argTy
        let func = addFunction myModule name funcTy

        // Set the function parameter names and fill the map
        let paramMapSeq = Seq.map (fun i -> 
            let paramName = params.[i].Name
            let llvmParam = getParam func <| uint32 i
            // Set the function param name and return a tuple for the map
            setValueName llvmParam paramName
            (paramName, llvmParam)) [0..numParams-1]

        globalFuncs.Add(name, new Func(name, func, Map.ofSeq paramMapSeq))

let gen (program:list<DeclA>) =
    let myModule = moduleCreateWithName "basicModule"
    
    let printfTy = varArgFunctionType i32 [| i8p |]
    let printf = addGlobalExtern myModule "printf" printfTy

    // Add %d numFmt
    addGlobalStringConstant myModule "numFmt" "%d\n" |> ignore

    // Make funcTypes for each procedure
    Seq.iter (defineDecl myModule) program

    // Generate code for each proc
    Seq.iter genDecl program

    myModule