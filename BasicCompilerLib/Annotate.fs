module Annotate

open System.Collections.Generic
open Print
open Tree
open Builtins

type AnnotateEnviron() =
    let vars = new Dictionary<string, PType>();
    let funcMod func = "<>func_" + func

    member x.GetVarType(var) = vars.[var]
    member x.SetVarType(var, ptype) = vars.[var] <- ptype

    member x.GetFunctionType(func) = vars.[funcMod func]
    member x.SetFunctionType(func, ptype) = vars.[funcMod func] <- ptype

    member x.Clone() =
        let ae = new AnnotateEnviron()
        Seq.iter (fun (kvp:KeyValuePair<string, PType>) -> ae.SetVarType(kvp.Key, kvp.Value) |> ignore) vars
        ae

let rec annotateTypesExpr (env:AnnotateEnviron) (exprA:ExprA) = 
    exprA.PType <- match exprA.Item with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | Var n -> env.GetVarType(n)
        | Binop (op, l, r) ->
            annotateTypesExpr env l
            annotateTypesExpr env r
            let type_ = binopToType op l.PType r.PType
            if type_ = None then
                failwithf "Incorrect type for binary operation:\n%s" (fmt exprA.Item)
            else
                type_.Value
        | Call (name, exprAs) ->
            Seq.iter (annotateTypesExpr env) exprAs
            env.GetFunctionType(name)

let rec annotateTypesStmt env (stmtA:StmtA) = 
    stmtA.PType <- match stmtA.Item with
        | Print exprA ->
            annotateTypesExpr env exprA
            Unit
        | Assign (var, exprA) ->
            annotateTypesExpr env exprA
            env.SetVarType(var, exprA.PType)
            exprA.PType
        | Return exprA ->
            annotateTypesExpr env exprA
            Unit
        | If (exprA, then_, else_) ->
            annotateTypesExpr env exprA
            Seq.iter (annotateTypesStmt env) then_
            Seq.iter (annotateTypesStmt env) else_
            Unit

let annotateTypesDecl (env:AnnotateEnviron) (declA:DeclA) = match declA.Item with
    | Proc (_, params_, _, stmts) -> 
        let fenv = env.Clone()
        Seq.iter (fun (p:Param) -> fenv.SetVarType(p.Name, p.PType) |> ignore) params_
        Seq.iter (annotateTypesStmt fenv) stmts


let rec annotateVarsExpr refs (exprA:ExprA) =
    exprA.AddRefs(refs)
    let annotateExpr = annotateVarsExpr refs
    match exprA.Item with
        | Binop (_, l, r) -> 
            annotateExpr l
            annotateExpr r
        | Call (_, exprAs) -> Seq.iter annotateExpr exprAs
        | _ -> ()

let rec annotateVarsStmt (declA:DeclA) (stmtA:StmtA) =
    stmtA.AddRefs(declA.Refs)
    let annotateExpr = annotateVarsExpr stmtA.Refs
    match stmtA.Item with
        | Assign (name, exprA) ->
            declA.AddRef(Ref(name, exprA.PType))
            annotateExpr exprA
        | Print exprA -> annotateExpr exprA
        | Return exprA -> annotateExpr exprA
        | If (exprA, thenStmtAs, elseStmtAs) ->
            annotateExpr exprA
            Seq.iter (annotateVarsStmt declA) thenStmtAs
            Seq.iter (annotateVarsStmt declA) elseStmtAs

let annotateVarsDecl (declA:DeclA) = match declA.Item with
    | Proc (_, prms, _, stmts) ->
        // Add the parameters
        Seq.iter (fun (p:Param) -> declA.AddRef(Ref(p.Name, p.PType))) prms
        // Propagate params to stmts
        Seq.iter (annotateVarsStmt declA) stmts
    

let annotate (program:Program) =

    // Create initial environment where all the functions return types have been added
    let env0 = new AnnotateEnviron()
    Seq.fold (fun (env:AnnotateEnviron) (declA:DeclA) ->
        match declA.Item with
            | Proc (name, _, returnType, _) ->
                declA.PType <- returnType
                env.SetFunctionType(name, returnType)
        env) env0 program
    |> ignore

    // Do a series of annotation passes
    Seq.iter (fun d -> 
        annotateTypesDecl env0 d
        annotateVarsDecl d) program
    program