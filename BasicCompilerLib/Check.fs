module Check

open Print
open Exceptions
open Tree

let isReturn = function
    | Return _ -> true
    | _ -> false

let getReturnExpr = function
    | Return e -> e
    | _ -> failwith "Need return stmt"