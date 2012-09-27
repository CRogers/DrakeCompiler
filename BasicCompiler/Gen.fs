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

let mutable icount = 0
let getTmp () =
    let ret = icount
    icount <- icount + 1
    "tmp" + ret.ToString()

let mkConst x = constInt i32 (uint64 x) false 

let rec genExpr bldr (func: Func) = function
    | Int x ->
        mkConst x
    | Binop (op, left, right) ->
        let buildFunc = match op with
            | Add -> buildAdd
            | Sub -> buildSub
            | Div -> buildSDiv
            | Mul -> buildMul
        buildFunc bldr (genExpr bldr func left) (genExpr bldr func right) (getTmp())
    | Call (name, args) ->
        let funcToCall = globalFuncs.[name]
        let argRefs = Seq.map (genExpr bldr func) args |> Seq.toArray
        buildCall bldr funcToCall.Func argRefs (getTmp())
    | Var name ->
        func.Params.[name]

let genStmt bldr func = function
    | Print e -> 
        let expr = genExpr bldr func e
        let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] (getTmp())
        buildCall bldr globals.["printf"] [| gep; expr |] <| getTmp()
    | Return e ->
        let expr = genExpr bldr func e
        buildRet bldr expr


let genDecl = function
    | Proc (name, numArgs, stmts) ->
        let func = globalFuncs.[name]
        use bldr = new Builder()
        positionBuilderAtEnd bldr (appendBasicBlock func.Func "entry")
        Seq.iter (fun s -> genStmt bldr func s |> ignore) stmts

let defineDecl myModule = function
    | Proc (name, params, _) ->
        let numParams = params.Length
        let argTy = Array.create numParams i32
        let funcTy = functionType i32 argTy
        let func = addFunction myModule name funcTy

        let paramMapArray = Array.zeroCreate<string * ValueRef> numParams

        // Set the function parameter names and fill the map
        let paramMapSeq = Seq.map (fun i -> 
            let paramName = params.[i]
            let llvmParam = getParam func <| uint32 i
            // Set the function param name
            setValueName llvmParam paramName
            (paramName, llvmParam)) [0..numParams-1]

        globalFuncs.Add(name, new Func(name, func, Map.ofSeq paramMapSeq))

let gen program =
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

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName