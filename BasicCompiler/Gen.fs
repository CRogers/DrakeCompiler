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
let getIcount () =
    let ret = icount
    icount <- icount + 1
    "tmp" + ret.ToString()

let mkConst x = constInt i32 (uint64 x) false 

let rec genExpr bldr func = function
    | Int x ->
        mkConst x
    | Binop (op, left, right) ->
        let buildFunc = match op with
            | Add -> buildAdd
            | Sub -> buildSub
            | Div -> buildSDiv
            | Mul -> buildMul
        buildFunc bldr (genExpr bldr func left) (genExpr bldr func right) (getIcount())
    | Call (name, args) ->
        let func = globals.[name]
        let argRefs = Seq.map (genExpr bldr func) args |> Seq.toArray
        buildCall bldr func argRefs (getIcount())

let genStmt bldr func = function
    | Print e -> 
        let expr = genExpr bldr func e
        let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] (getIcount())
        buildCall bldr globals.["printf"] [| gep; expr |] <| getIcount()
    | Return e ->
        let expr = genExpr bldr func e
        buildRet bldr expr


let genDecl = function
    | Proc (name, numArgs, stmts) ->
        let func = globals.[name]
        use bldr = new Builder()
        positionBuilderAtEnd bldr (appendBasicBlock func "entry")
        Seq.iter (fun s -> genStmt bldr func s |> ignore) stmts

let defineDecl myModule = function
    | Proc (name, params, _) ->
        let numParams = params.Length
        let argTy = Seq.initInfinite (fun i -> i32) |> Seq.take numParams |> Seq.toArray
        let funcTy = functionType i32 argTy
        let func = addFunction myModule name funcTy

        // Set the function parameter names
        Seq.iter (fun i -> setValueName (getParam func <| uint32 i) params.[i]) [0..numParams-1]

        globals.Add(name, func)

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