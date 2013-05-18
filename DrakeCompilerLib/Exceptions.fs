module Exceptions

open System
open System.IO
open Microsoft.FSharp.Text.Lexing
open Tree

type Error(message:string, pos:Pos) =
    member x.Message = message
    member x.Pos = pos

let trimTabs (str:string) = str.TrimStart([|'\t'|])

let appendN n char str = str + ("".PadRight(n, char))

let errorText textLines (error:Error) =
    let s = error.Pos.StartPos.Line, error.Pos.StartPos.Column
    let e = error.Pos.EndPos.Line, error.Pos.EndPos.Column
    let (sline, scol) = s
    let (eline, ecol) = e
    let strStart = sprintf "line %d, column %d: %s\n" (sline+1) scol error.Message
    let sourceLines = Seq.take (eline-sline+1) <| Seq.skip sline textLines
    let printThingy lineno line =
        let untabbedLine = trimTabs line
        let ntabs = line.Length - untabbedLine.Length
        let ntabspaces = ntabs*2
        let tabspacer = "".PadRight(ntabspaces)
        let spos = if lineno = sline then max 0 (scol-ntabspaces) else 0
        let epos = if lineno = eline then max 0 (ecol-ntabspaces) else max 0 (line.Length-ntabspaces)
        let markerLength = epos - spos
        let marker = appendN markerLength '^' <| "".PadRight(spos)
        sprintf "%s%s\n%s%s\n" tabspacer untabbedLine tabspacer marker
    let markerLines = Seq.mapi (fun i line -> printThingy (i+sline) line) sourceLines
    strStart + String.Concat(Seq.toArray markerLines)
    
let printError textLines (error:Error) =
    Console.Write(errorText textLines error)

let innerExnOption innerException = match innerException with | Some(ex) -> ex | _ -> null

type CompilerException(message:string, ?innerException:exn) =
    inherit ApplicationException (message, innerExnOption innerException)

type LexerException(message:string, ?innerException:exn) =
    inherit CompilerException(message, innerExnOption innerException)

type ParserException(message:string, ?innerException:exn) =
    inherit CompilerException(message, innerExnOption innerException)

type AnnotationException(message:string, ?innerException:exn) =
    inherit CompilerException(message, innerExnOption innerException)

type CheckException(message:string, ?innerException:exn) =
    inherit CompilerException(message, innerExnOption innerException)
