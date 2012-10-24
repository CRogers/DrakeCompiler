module Gen

open System.Collections.Generic
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let i1 = int1Type ()
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

let boolToI1 x = constInt i1 (if x then 1UL else 0UL) false

let typeToLLVMType t = match t with
    | Unit -> i32
    | Bool -> i1
    | Int  -> i32

let rec genExpr bldr (env:Environ) irname (exprA:ExprA) =
    let llvmname = if irname.Equals("") then env.GetTmp() else env.GetName irname
    match exprA.Item with
        | ConstInt x ->
            mkConst x
        | ConstBool b ->
            boolToI1 b
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
            buildFunc bldr (genExpr bldr env "" left) (genExpr bldr env "" right) llvmname
            
        | Call (name, args) ->
            let funcToCall = globalFuncs.[name]
            let argRefs = Seq.map (genExpr bldr env "") args |> Seq.toArray
            buildCall bldr funcToCall.Func argRefs llvmname
        | Var name ->
            env.GetRef(name)

let genStmt bldr (env:Environ) (stmtA:StmtA) =
    match stmtA.Item with
        | Print e -> 
            let expr = genExpr bldr env "print" e
            let mexpr = if e.PType = Bool then buildZExt bldr expr i32 "convert" else expr
            let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] (env.GetTmp())
            buildCall bldr globals.["printf"] [| gep; mexpr |] <| env.GetTmp()
        | Assign (name, e) ->
            let expr = genExpr bldr env name e
            env.AddRef(name, expr)
            expr
        | Return e ->
            let expr = genExpr bldr env "return" e
            let ret = buildRet bldr expr
            let md = mDString ("cat" + System.DateTime.Now.Ticks.ToString()) 3u
            ret 


let genDecl module_ (declA:DeclA) = match declA.Item with
    | Proc (name, _, _, stmts) ->
        let func = globalFuncs.[name]
        let env = Environ(module_, func)
        use bldr = new Builder()
        positionBuilderAtEnd bldr (appendBasicBlock func.Func "entry")
        Seq.iter (fun s -> genStmt bldr env s |> ignore) stmts

let defineDecl myModule (declA:DeclA) = match declA.Item with
    | Proc (name, params, returnType, _) ->
        let numParams = params.Length
        let argTy = Seq.toArray <| Seq.map (fun (t:Param) -> typeToLLVMType t.PType) params
        let returnTy = typeToLLVMType returnType
        let funcTy = functionType returnTy argTy
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
    Seq.iter (genDecl myModule) program

    myModule