module LLVM_Test

open System
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let makeModule () =
    let myModule = moduleCreateWithName "myModule"        
    let tyInt = int32Type ()
    let tyFunc = functionType tyInt [| tyInt; tyInt |]
    let fSum = addFunction myModule "sum" tyFunc

    setValueName (getParam fSum 0u) "a"
    setValueName (getParam fSum 1u) "b"

    let builder = createBuilder ()
    positionBuilderAtEnd builder (appendBasicBlock fSum "entry")
    buildAdd builder (getParam fSum 0u) (getParam fSum 1u) "tmp"
    |> buildRet builder
    |> ignore

    writeBitcodeToFile myModule "my-module.bs"