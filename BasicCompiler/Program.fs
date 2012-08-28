module Program

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
            Tree.Int 0


[<EntryPointAttribute>]
let main args =
    let mutable file = "../../tests/test.bco"
    if args.Length > 0 then
        file <- args.[0]

    lex (fileToLexbuf file)
    printfn "\n"
    printfmt (parse file)

    0