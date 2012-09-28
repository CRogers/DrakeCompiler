module Compiler

open Tree
open Print
open Printf
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing
open System
open System.Text
open System.IO
open LLVM.Generated.BitWriter


let fileToLexbuf file =
    let stream = File.OpenText(file)
    LexBuffer<_>.FromTextReader stream

let stringToLexbuf str = 
    LexBuffer<_>.FromString str

let lex (lexbuf:LexBuffer<_>) =
    let sb = new StringBuilder()
    while not lexbuf.IsPastEndOfStream do
        sb.Append(Lexer.token lexbuf |> fmt) |> ignore
    sb.ToString()

let lexText text = lex <| stringToLexbuf text
let lexFile file = lex <| fileToLexbuf file

let trimTabs (str:string) = str.TrimStart(Seq.toArray ['\t'])

let parse (text: array<string>) =
    let lexbuf = stringToLexbuf <| String.Join ("\n", text)
    try
        Parser.program Lexer.token lexbuf
    with
        | ex ->
            let line = lexbuf.StartPos.Line 
            let col = lexbuf.StartPos.Column
            printfn "Parse error at line %d, column %d at token %s" (line+1) col (Lexer.lexeme lexbuf)
            let sourceLine = Seq.nth line text
            let unpaddedLine = trimTabs sourceLine
            let unpaddedSpacer = "".PadRight(col - (sourceLine.Length - unpaddedLine.Length))
            printfn "%s\n%s^" unpaddedLine unpaddedSpacer
            []

let parseFile file = parse (File.ReadAllLines file)
let parseText (text: string) = parse <| text.Split('\n')

let compile text =
    let parsed = parseText text
    Gen.gen parsed

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName