module Compiler

open Tree
open Print
open Exceptions
open Printf
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing
open System
open System.Text
open System.IO
open LLVM.Generated.Core
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
        sb.Append(' ') |> ignore
    sb.ToString()

let lexText text = lex <| stringToLexbuf text
let lexFile file = lex <| fileToLexbuf file

let parse (text: array<string>) =
    let lexbuf = stringToLexbuf <| String.Join ("\n", text)
    try
        Parser.program Lexer.token lexbuf
    with
        | ex ->
            let error = Error("Parse error", Pos(lexbuf.StartPos, lexbuf.EndPos))
            printError text error
            failwithf "bleh"

let parseFile file = parse (File.ReadAllLines file)
let parseText (text: string) = parse <| text.Split('\n')

type CompilerResult(textLines: array<string>, errors: list<Error>, llvmModule: ModuleRef) =
    member x.Errors = errors
    member x.Success = errors.Length = 0
    member x.PrintErrors() = Seq.iter (printError textLines) errors
    member x.GetErrorText() = Seq.map (errorText textLines) errors
    member x.Module = llvmModule

let compile (text:string) =
    try
        let textLines = text.Split('\n')
        let parsed = [parse textLines]
        let (globals, annotated) = Annotate.annotate parsed
        let llvmModule = Gen.gen globals annotated
        CompilerResult(textLines, [], llvmModule)
    with
        | ex -> 
            printfn "Exception message: %s" ex.Message
            printfn "Stacktrace: \n%s" ex.StackTrace
            if System.Diagnostics.Debugger.IsAttached then
                reraise ()
            else
                CompilerResult([||], [], moduleCreateWithName "fail")

let writeModuleToFile fileName mo = writeBitcodeToFile mo fileName