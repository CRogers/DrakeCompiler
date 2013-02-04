module Annotate

(*

Glues together all the separate annotate modules

*)

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

let annotate (program:Program) =
    let globals = getStdRefs program

    // Flattern tree to class/interface level
    let flatProg = flatternAST program

    annotateTypes globals flatProg

    flatProg