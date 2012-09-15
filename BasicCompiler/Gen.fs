module Gen

open System.Collections.Generic
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter
open LLVM.Extras

let tyInt = int32Type ()
let tyVoid = voidType ()
let i8 = int8Type ()
let tyPByte = pointerType (int8Type ()) 0u

let globals = new Dictionary<string, ValueRef>()
let addGlobalExtern mo name funcTy =
    let func = addFunction mo name funcTy
    //addFunctionAttr func Attribute.ZExtAttribute
    globals.Add(name, func)
    func

let addGlobalStringConstant mo name (str:string) =
    let len = uint32 (str.Length + 1)
    let glob = addGlobal mo (arrayType i8 len) name
    setLinkage glob Linkage.InternalLinkage
    setGlobalConstant glob true
    setInitializer glob (constString str len false)
    globals.Add(name, glob)
    glob

let mutable icount = 0
let getIcount () =
    let ret = icount
    icount <- icount + 1
    ret.ToString()

let mkConst x = constInt tyInt (uint64 x) false 

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
    | Print e -> genExpr bldr e
        //buildCall bldr globals.["printf"] [| globals.["numFmt"]; (genExpr bldr e) |] <| getIcount()

let gen program =
    let myModule = moduleCreateWithName "basicModule"
    
    let printfTy = varArgFunctionType tyInt [| tyPByte |]
    let printf = addGlobalExtern myModule "printf" printfTy

    // Add %d numFmt
    addGlobalStringConstant myModule "numFmt" "%d" |> ignore

    let mainTy = functionType tyInt [| tyInt |]
    let main = addFunction myModule "main" mainTy

    use bldr = new Builder()
    positionBuilderAtEnd bldr (appendBasicBlock main "entry")
    
    Seq.iter (fun s -> genStmt bldr s |> ignore) program 
    
    buildRet bldr (mkConst 0) |> ignore

    myModule

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName