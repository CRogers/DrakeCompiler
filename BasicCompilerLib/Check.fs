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
    getClassDecls program
    |> Seq.iter (fun cA -> match cA.Item with
        | ClassProc (name, vis, isStatic, params_, ptype, eA) when !ptype = commonPtype Unit ->
            let penultimate, last = lastInSeqAndPrev eA
            // If we find no return at the end of the function, add one
            if not <| isReturnVoid last then
                match penultimate with
                    | None -> failwithf "Something has gone terribly wrong"
                    | Some eA -> match eA.Item with
                        | Seq (_, e2A) ->
                            e2A := ExprA(Seq(ref last, ref <| returnVoidA last.Refs), Pos.NilPos)

        | ClassProc (name, vis, isStatic, params_, ptype, eA) ->
            iterAST foldASTExpr (fun (annot:Annot) ->
                let eA = annot :?> ExprA
                match eA.Item with
                    | ReturnVoid -> failwithf "Must return %s not void" <| (!ptype).ToString()
                    | Return reA -> if not (reA.PType = !ptype) then failwithf "Must return %s not %s" ((!ptype).ToString()) (reA.ToString())
                    | _ -> ()) eA

            let last = lastInSeq eA
            if not <| isReturnTyped last then
                failwithf "Must return at some point"
        | _ -> ()
    )

let check (program:seq<NamespaceDeclA>) =
    checkReturns program