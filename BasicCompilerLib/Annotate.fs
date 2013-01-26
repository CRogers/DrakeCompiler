module Annotate

(*

Glues together all the separate annotate modules

*)

open Tree
open Annotate1
open Annotate2

let annotate (program:Program) =
    getStdRefs program
    annotateTypes program