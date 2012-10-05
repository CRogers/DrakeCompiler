module Annotate

open Print
open Tree

let rec annotateTypesExpr (exprA:ExprA) = 
    exprA.PType <- match exprA.Item with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | Var _ -> Int
        | Binop (op, l, r) ->
            annotateTypesExpr l
            annotateTypesExpr r
            if l.PType <> r.PType then
                failwithf "Both sides of the binary operation need to have the same type:\n%s" (fmt exprA.Item)
            else
                l.PType
        | Call (_, exprAs) ->
            Seq.iter annotateTypesExpr exprAs
            Int

let annotateTypesStmt (stmtA:StmtA) = 
    stmtA.PType <- match stmtA.Item with
        | Print exprA ->
            annotateTypesExpr exprA
            Unit
        | Assign (_, exprA) -> 
            annotateTypesExpr exprA
            exprA.PType
        | Return exprA ->
            annotateTypesExpr exprA
            Unit

let annotateTypesDecl (declA:DeclA) = match declA.Item with
    | Proc (_, _, _, stmts) -> Seq.iter annotateTypesStmt stmts


let rec annotateVarsExpr refs (exprA:ExprA) =
    exprA.AddRefs(refs)
    let annotateExpr = annotateVarsExpr refs
    match exprA.Item with
        | Binop (_, l, r) -> 
            annotateExpr l
            annotateExpr r
        | Call (_, exprAs) -> Seq.iter annotateExpr exprAs
        | _ -> ()

let annotateVarsStmt (declA:DeclA) (stmtA:StmtA) =
    stmtA.AddRefs(declA.Refs)
    let annotateExpr = annotateVarsExpr stmtA.Refs
    match stmtA.Item with
        | Assign (name, exprA) ->
            declA.AddRef(Ref(name, exprA.PType))
            annotateExpr exprA
        | Print exprA -> annotateExpr exprA
        | Return exprA -> annotateExpr exprA

let annotateVarsDecl (declA:DeclA) = match declA.Item with
    | Proc (_, prms, _, stmts) ->
        // Add the parameters
        Seq.iter (fun (p:Param) -> declA.AddRef(Ref(p.Name, p.PType))) prms
        // Propagate params to stmts
        Seq.iter (annotateVarsStmt declA) stmts
    

let annotate (program:Program) = 
    Seq.iter (fun d -> 
        annotateTypesDecl d
        annotateVarsDecl d) program
    program