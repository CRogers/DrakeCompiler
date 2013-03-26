module Builtins

open Tree
open LLVMTypes
open LLVM.Core
open LLVM.Generated.Core

let getIntNA globals size = Map.find ("System::Int" + size.ToString()) globals

let builtinSetUpTypes (globals:GlobalStore) =
    let setIntType size =
        let nA = getIntNA globals size
        nA.InstanceType <- Some <| intSizeToTy size

    Seq.iter setIntType [8;16;32;64]

    let bool = Map.find "System::Bool" globals
    bool.InstanceType <- Some i1

    let console = Map.find "System::Console" globals
    console.InstanceType <- Some tyVoid

    let unit = Map.find "System::Unit" globals
    unit.InstanceType <- Some tyVoid


let builtinGenInts (globals:GlobalStore) =
    let genInt (size:int) =
        let nA = getIntNA globals size
        let buildOp (name, buildFunc) =
            let cA = match nA.GetRef(name) with Some (ClassRef cA) -> cA
            let funcvr = cA.Ref.ValueRef

            use bldr = new Builder()
            let entry = appendBasicBlock funcvr "entry"
            positionBuilderAtEnd bldr entry

            let param n =
                let x = getParam funcvr n
                buildBitCast bldr x nA.InstanceType.Value ""

            let a = param 0u
            let b = param 1u
            
            buildFunc bldr a b ""
            |> buildRet bldr
            |> ignore

        [
            ("+", buildAdd);
            ("-", buildSub);
            ("*", buildMul); 
            ("/", buildSDiv);
            ("%", buildSRem);
            ("&", buildAnd);
            ("|", buildOr);
        ]
        |> Seq.iter buildOp 

    Seq.iter genInt [8;16;32;64]

let builtinGenBool (globals:GlobalStore) =
    let nA = Map.find "System::Bool" globals
    let buildOp (name, buildFunc) =
        let cA = match nA.GetRef(name) with Some (ClassRef cA) -> cA
        let funcvr = cA.Ref.ValueRef

        use bldr = new Builder()
        let entry = appendBasicBlock funcvr "entry"
        positionBuilderAtEnd bldr entry

        let a = getParam funcvr 0u
        let b = getParam funcvr 1u

        buildFunc bldr a b ""
        |> buildRet bldr
        |> ignore

    [
        ("&&", buildAnd);
        ("||", buildOr);
    ]
    |> Seq.iter buildOp


let builtinGenConsole externs (globals:GlobalStore) =
    let printf = Map.find "printf" externs
    let numFmt = Map.find "numFmt" externs

    let nA = Map.find "System::Console" globals
    let printlnCA = match nA.GetRef("println") with Some (ClassRef cA) -> cA
    let println = printlnCA.Ref.ValueRef

    let entry = appendBasicBlock println "entry"
    use bldr = new Builder()
    positionBuilderAtEnd bldr entry

    let numFmtGEP = buildGEP bldr numFmt [|i32zero; i32zero|] ""

    buildCall bldr printf [|numFmtGEP; getParam println 0u|] "" |> ignore
    buildRetVoid bldr |> ignore