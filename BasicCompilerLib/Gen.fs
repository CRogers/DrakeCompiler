module Gen

open System
open Tree
open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.BitWriter

let i1 = int1Type ()
let i8 = int8Type ()
let i16 = int16Type ()
let i32 = int32Type ()
let i64 = int64Type ()
let tyVoid = voidType ()
(*
let genClassStructure

let genNamespaceDecl mo (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, cAs) ->
            genClassStructures mo nA
*)

let gen (program:seq<NamespaceDeclA>) =
    let mo = moduleCreateWithName <| DateTime.Now.ToLongDateString()
    mo