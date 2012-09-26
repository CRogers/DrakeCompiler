module Program

open Tree
open Print
open Printf
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing
open System
open System.IO


let fileToLexbuf file =
    let stream = File.OpenText(file)
    LexBuffer<_>.FromTextReader stream

let stringToLexbuf str = 
    LexBuffer<_>.FromString str

let lex (lexbuf:LexBuffer<_>) =
    while not lexbuf.IsPastEndOfStream do
        printf "%s " (Lexer.token lexbuf |> fmt)

let trimTabs (str:string) = str.TrimStart(Seq.toArray ['\t'])

let parse file =
    let lexbuf = fileToLexbuf file
    try
        Parser.program Lexer.token lexbuf
    with
        | ex ->
            let line = lexbuf.StartPos.Line 
            let col = lexbuf.StartPos.Column
            printfn "Parse error at line %d, column %d at token %s" (line+1) col (Lexer.lexeme lexbuf)
            let sourceLine = Seq.nth line (File.ReadLines(file))
            let unpaddedLine = trimTabs sourceLine
            let unpaddedSpacer = "".PadRight(col - (sourceLine.Length - unpaddedLine.Length))
            printfn "%s\n%s^" unpaddedLine unpaddedSpacer
            []

let printFile name = printfn "%s" <| File.ReadAllText(name)

[<EntryPointAttribute>]
let main args =

    let mutable file = "../../tests/test.bco"
    if args.Length > 0 then
        file <- args.[0]

    printFile file

    lex (fileToLexbuf file)
    printfn "\n"
    let parsed = (parse file)
    printfmt parsed

    Gen.gen parsed
    |> Gen.writeModuleToFile "test.bc"
    |> ignore

    printfn "\n"
    let disassembler = System.Diagnostics.Process.Start("llvm-dis", "test.bc")
    disassembler.WaitForExit()
    printFile("test.ll");

    Console.ReadLine() |> ignore

    0