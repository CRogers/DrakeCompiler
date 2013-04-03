module Annotate

(*

Glues together all the separate annotate modules

*)

open Util
open Tree
open AnnotateRefs
open AnnotateInterfaces
open AnnotateTypes

let flatternAST (program:Program) =
    let flatternTop (tA:TopDeclA) =
        match tA.Item with
            | Namespace (name, nAs) -> nAs
            | _ -> []
    let flatternCU cu = concatMap flatternTop cu
    concatMap flatternCU program
    |> List.ofSeq


// The "this" parameter at the start of instance methods is implicit - we must add it
let fixNonStaticFunctionParams (nAs:list<NamespaceDeclA>) =
    let classProcs = getClassDecls nAs
    Seq.iter (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc (_, _, NotStatic, params_, _, _) ->
            params_ := Param("this", Type <| cA.NamespaceDecl.Value) :: !params_
        | _ -> ()) classProcs


let findBinops (nAs:list<NamespaceDeclA>) =
    getClassDecls nAs
    |> Seq.map (fun cA -> match cA.Item with
        | ClassProc (name, _, Static, params_, _, _) when (!params_).Length = 2 ->
            Some (classNPKey cA, cA)
        | _ -> None)
    |> Util.getSomes
    |> Seq.groupBy fst
    |> Seq.map (fun (npkey, seqNslCA) -> (npkey, List.ofSeq <| Seq.map snd seqNslCA))
    |> Map.ofSeq

type Test = 
    | Foo
    | Bar
    with override x.ToString() = "cat"

let annotate (program:Program) =
    let globals = getGlobalRefs program

    // Flattern tree to class/interface level
    let flatProg = flatternAST program

    annotateCIRefs globals flatProg
    annotateInterfaces globals flatProg

    // Find the binops
    let binops = findBinops flatProg

    annotateTypes globals binops flatProg

    fixNonStaticFunctionParams flatProg

    (globals, flatProg) 