module Program

open Compiler
open Print
open System.IO

let printFile name = printfn "%s" <| File.ReadAllText(name)

let main (args:array<string>) =

    let mutable file = "../../../tests/test.bco"
    if args.Length > 0 then
        file <- args.[0]

    printFile file

    lexFile file
    printfn "\n"
    let parsed = parseFile file
    printfmt parsed

    Gen.gen parsed
    |> writeModuleToFile "test.bc"
    |> ignore

    printfn "\n"
    let disassembler = System.Diagnostics.Process.Start("llvm-dis", "test.bc")
    disassembler.WaitForExit()
    printFile("test.ll");

    0