module Gen

open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let tyInt = int32Type ()

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

let gen expr =
    let myModule = moduleCreateWithName "basicModule"
    let tyFunc = functionType tyInt [| tyInt |]
    let main = addFunction myModule "main" tyFunc

    use bldr = new Builder()
    positionBuilderAtEnd bldr (appendBasicBlock main "entry")
    
    genExpr bldr expr
    |> buildRet bldr
    |> ignore

    myModule

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName