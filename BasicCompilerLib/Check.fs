module Check

open Print
open Exceptions
open Tree

let getReturnExpr = function
    | Return e -> e
    | _ -> failwith "Need return stmt"

let returnVoidA (refs:Map<string,Ref>) =
    let rvA = ExprA(ReturnVoid, Pos.NilPos)
    rvA.PType <- commonPtype Unit
    rvA.AddRefs(refs)
    rvA

let checkReturns (program:seq<NamespaceDeclA>) =
    let rec isBlocked eA =
        let foundIsBlocked (eA:ExprA) = match eA.Item with
            | ReturnVoid
            | Return _ -> true
            | If (test, then_, else_) -> isBlocked then_ && isBlocked else_

        let blocker = findInSeq (fun eA -> match eA.Item with | If _ | Return _ | ReturnVoid -> true | _ -> false) eA
        match blocker with
            | NotFound -> false
            | FoundInMiddle (seq, blocker) ->
                let ib = foundIsBlocked blocker
                if ib then
                    failwithf "Dead code detected"
                else
                    isBlocked (match seq.Item with Seq (_, next) -> !next)
            | FoundAtEnd eA -> foundIsBlocked eA

    getClassDecls program
    |> Seq.iter (fun cA -> match cA.Item with
        | ClassProc (name, vis, isStatic, params_, ptype, eA) when !ptype = commonPtype Unit ->
            // Check there are no returns of the wrong type
            iterAST foldASTExpr (fun (annot:Annot) ->
                let eA = annot :?> ExprA
                match eA.Item with
                    | Return reA -> failwithf "Can only use void return"
                    | _ -> ()) eA

            // If we find no return at the end of the function, add one
            if not <| isBlocked eA then
                let penultimate, last = lastInSeqAndPrev eA
                match penultimate with
                    | None -> failwithf "Something has gone terribly wrong"
                    | Some eA -> match eA.Item with
                        | Seq (_, e2A) ->
                            e2A := ExprA(Seq(ref last, ref <| returnVoidA last.Refs), Pos.NilPos)

        | ClassProc (name, vis, isStatic, params_, ptype, eA) ->
            // Make sure all returns have the right type
            iterAST foldASTExpr (fun (annot:Annot) ->
                let eA = annot :?> ExprA
                match eA.Item with
                    | ReturnVoid -> failwithf "Must return %s not void" <| (!ptype).ToString()
                    | Return reA -> if not (reA.PType = !ptype) then failwithf "Must return %s not %s" ((!ptype).ToString()) (reA.ToString())
                    | _ -> ()) eA

            if not <| isBlocked eA then
                failwithf "Must return at end"
        | _ -> ()
    )

let check (program:seq<NamespaceDeclA>) =
    checkReturns program