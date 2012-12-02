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

let checkReturnStmt (declA:DeclA) =
    match declA.Item with
        | Proc (name, prms, returnType, body) ->
            // Check that all return statements return the correct type
            foldAST (fun branch xss ->
                let xs = List.concat xss
                match branch.Item with
                    | Return e -> if branch.PType = returnType then xs else xs @ [Error(sprintf "Incorrect return type for return expr. Expected type %s, got %s" (fmt returnType) (fmt branch.PType), branch.Pos)] 
                    | _ -> xs) (fun x -> []) body

let check program =
    let concatMap f list = List.concat <| List.map f list
    let errors = concatMap checkReturnStmt program
    errors