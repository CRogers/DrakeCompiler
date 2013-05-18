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
let fixNonStaticFunctionParams (nAs:seq<NamespaceDeclA>) =
    let classProcs = getCIClassDecls nAs
    Seq.iter (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc (_, _, NotStatic, params_, _, _) ->
            params_ := Param("this", Type <| cA.NamespaceDecl.Value) :: !params_
        | _ -> ()) classProcs


let findBinops (nAs:seq<NamespaceDeclA>) =

    let groupedByKey = 
        getClassDecls nAs
        |> Seq.map (fun cA -> 
            if cA.IsBinop then Some (classNPKey cA, ClassRef cA)
            else None)
        |> Util.getSomes
        |> Seq.groupBy fst
        |> Seq.map (fun (npkey, seqNslCA) -> (npkey, Seq.map snd seqNslCA))

    // See if there are any that are declared twice
    for k, v in groupedByKey do
        if not (Seq.length v = 1) then
            failwithf "Multiple definitions of binop %s" (NPKeyPretty k)

    groupedByKey
    |> Seq.map (fun (key, cAs) -> (key, Seq.head cAs))
    |> Map.ofSeq

let annotate (program:Program) =
    let globals = getGlobalRefs program

    Templates.annotateTypeParams <| globalsToNAs !globals
  
    expandTypes !globals

    Mixins.fixMixinReferences <| globalsToNAs !globals
    Templates.expandTemplates globals
    Mixins.makeForwardingMethods <| globalsToNAs !globals

    // Find the binops
    let binops = findBinops <| globalsToNAs !globals

    annotateInterfaces  <| globalsToNAs !globals

    annotateTypes globals binops

    fixNonStaticFunctionParams <| globalsToNAs !globals
    
    !globals 