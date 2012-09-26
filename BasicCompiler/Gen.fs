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

let rec genExpr bldr = function
    | Int x ->
        mkConst x
    | Binop (op, left, right) ->
        let buildFunc = match op with
            | Add -> buildAdd
            | Sub -> buildSub
            | Div -> buildSDiv
            | Mul -> buildMul
        buildFunc bldr (genExpr bldr left) (genExpr bldr right) (getIcount())

let genStmt bldr = function
    | Print e -> 
        let expr = genExpr bldr e
        let gep = buildGEP bldr (globals.["numFmt"]) [| i32zero; i32zero|] (getIcount())
        buildCall bldr globals.["printf"] [| gep; expr |] <| getIcount()

let gen program =
    let myModule = moduleCreateWithName "basicModule"
    
    let printfTy = varArgFunctionType i32 [| i8p |]
    let printf = addGlobalExtern myModule "printf" printfTy

    // Add %d numFmt
    addGlobalStringConstant myModule "numFmt" "%d\n" |> ignore

    let mainTy = functionType i32 [| i32 |]
    let main = addFunction myModule "main" mainTy

    use bldr = new Builder()
    positionBuilderAtEnd bldr (appendBasicBlock main "entry")
    
    Seq.iter (fun s -> genStmt bldr s |> ignore) program 
    
    buildRet bldr (mkConst 0) |> ignore

    myModule

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName