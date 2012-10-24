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
        | Proc (name, prms, returnType, stmtAs) ->
            if stmtAs.Length = 0 || not <| isReturn (Seq.last stmtAs).Item then
                [Error("The last statement in a prodcedure must be a return stmt", (Seq.last stmtAs).Pos)]
            else
                let returnStmt = Seq.last stmtAs
                let returnExpr = getReturnExpr returnStmt.Item
                if returnExpr.PType <> returnType then
                    [Error(sprintf "Incorrect return type. Expected type %s, got %s" (fmt returnType) (fmt returnExpr.PType), returnExpr.Pos)]
                else
                    []

let check program =
    let concatMap f list = List.concat <| List.map f list
    let errors = concatMap checkReturnStmt program
    errors