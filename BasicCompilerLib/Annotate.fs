module Annotate

(*

Glues together all the separate annotate modules

*)

open Util
open Tree
open Annotate1
open Annotate2

let flatternAST (program:Program) =
    let flatternTop (tA:TopDeclA) =
        match tA.Item with
            | Namespace (name, nAs) -> nAs
            | _ -> []
    let flatternCU cu = concatMap flatternTop cu
    concatMap flatternCU program


// The "this" parameter at the start of instance methods is implicit - we must add it
let fixNonStaticFunctionParams (nAs:seq<NamespaceDeclA>) =
    let classProcs = getClassDecls nAs
    Seq.iter (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc (_, _, NotStatic, params_, _, _) ->
            params_ := Param("this", UserType <| qualifiedName cA.Namespace cA.NamespaceDecl.Value.Name []) :: !params_
        | _ -> ()) classProcs


let annotate (program:Program) =
    let globals = getGlobalRefs program
    
    // Flattern tree to class/interface level
    let flatProg = flatternAST program

    annotateCIRefs globals flatProg
    annotateTypes globals flatProg

    fixNonStaticFunctionParams flatProg

    (globals, flatProg) 