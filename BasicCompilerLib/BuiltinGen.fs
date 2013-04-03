module BuiltinGen

open Tree
open LLVMTypes
open LLVM.Core
open LLVM.Generated.Core



let genConsole globals externs mo (nA:NamespaceDeclA) =
    let printf = Map.find "printf" externs
    let numFmt = Map.find "numFmt" externs
    
    let println = addFunction mo "Console__println" <| functionType tyVoid [|i32|]
    let entry = appendBasicBlock println "entry"
    use bldr = new Builder()
    positionBuilderAtEnd bldr entry

    let numFmtGEP = buildGEP bldr numFmt [|i32zero; i32zero|] ""

    buildCall bldr printf [|numFmtGEP; getParam println 0u|] "" |> ignore
    buildRetVoid bldr |> ignore

    match nA.GetRef(ProcKey ("println", [])).Value with
        | ClassRef cA -> 
            let ref = Ref("println", commonPtype globals Unit, StaticProcRef)
            ref.ValueRef <- Some println
            cA.Ref <- ref