module GenUtil

open Tree
open LLVM.Generated.Core
open LLVM.Core
open LLVMTypes

let getLLVMType ptype =
    match ptype with
        | Type nA -> nA.InstanceType.Value
        | _ -> failwithf "Can't find llvm type for %s" <| ptype.ToString()

let getNumFunctionParams funcvr =
    getParams funcvr
    |> Array.length


let changeSRO (text:string) = text.Replace("::", "__");

let getInstPointTy ptype =
    let nA = ptypeToNA ptype
    match nA.Item with
        | Class _ -> match nA.IsStruct with
            | Struct -> nA.InstanceType.Value
            | NotStruct -> nA.InstancePointerType.Value
        | Interface _ -> nA.InstancePointerType.Value


let genClassVarGEP bldr this offset = 
    buildStructGEP bldr this (uint32 offset) ""

let genClassVarStore bldr this offset value =
    let ptr = genClassVarGEP bldr this offset
    buildStore bldr value ptr

let genClassVarLoad bldr this offset =
    let ptr = genClassVarGEP bldr this offset
    buildLoad bldr ptr ""