module Annotate

open Print
open Tree

let rec annotateExpr (exprA:ExprA) = 
    exprA.PType <- match exprA.Expr with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | Var _ -> Int
        | Binop (op, l, r) ->
            annotateExpr l
            annotateExpr r
            if l.PType <> r.PType then
                failwithf "Both sides of the binary operation need to have the same type:\n%s" (fmt exprA.Expr)
            else
                l.PType
        | Call (_, exprAs) ->
            Seq.iter annotateExpr exprAs
            Int

let annotateStmt (stmtA:StmtA) = 
    stmtA.PType <- match stmtA.Stmt with
        | Print exprA ->
            annotateExpr exprA
            Unit
        | Assign (_, exprA) -> 
            annotateExpr exprA
            exprA.PType
        | Return exprA ->
            annotateExpr exprA
            Unit

let annotateDecl (declA:DeclA) = match declA.Decl with
    | Proc (_, _, _, stmts) -> Seq.iter annotateStmt stmts

let annotate (program:Program) = 
    Seq.iter annotateDecl program
    program