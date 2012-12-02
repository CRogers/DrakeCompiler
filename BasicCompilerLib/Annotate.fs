module Annotate

open System.Collections.Generic
open Print
open Tree
open Builtins

type Env(localVars:list<Ref>) =
    let mutable i = 0;
    let mutable localVars = localVars

    member x.LocalVars = localVars
    member x.AddLocalVar(ref) = localVars <- ref :: localVars
    member x.Count = i <- i + 1;

let rec annotateTypesExpr (env:Env) (exprA:ExprA) =    
    exprA.PType <- match exprA.Item with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | Var n -> exprA.GetRef(n).PType
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
            exprA.GetRef(name).PType
        | Assign (name, innerExprA) ->
            annotateTypesExpr env innerExprA
            innerExprA.PType
        | DeclVar (name, assignA) ->
            annotateTypesExpr env assignA
            // Since we have declared a variable, add a reference object for it
            let ref = Ref(name, assignA.PType, Local)
            env.AddLocalVar(ref)
            exprA.AddRef(ref)
            assignA.PType
        | Print exprA ->
            annotateTypesExpr env exprA
            exprA.PType
        | Return exprA ->
            annotateTypesExpr env exprA
            Unit
        | If (test, then_, else_) ->
            annotateTypesExpr env test
            annotateTypesExpr env then_
            annotateTypesExpr env else_
            then_.PType
        | While (exprA, stmtAs) ->
            annotateTypesExpr env exprA
            annotateTypesExpr env stmtAs
            stmtAs.PType
        | Seq (e1A, e2A) ->
            annotateTypesExpr env e1A
            // Since e2A is lexically below and and in the same scope as e1A, all 
            // e1A's references also appear in e2A
            e2A.AddRefs(e1A.Refs)
            annotateTypesExpr env e2A
            e2A.PType

let annotateTypesDecl (declA:DeclA) = match declA.Item with
    | Proc (name, params_, returnType, exprA) ->
        // Add the parameters to the body's environment
        Seq.iter (fun (p:Param) -> exprA.AddRef(Ref(p.Name, p.PType, Parameter))) params_
        let env = Env([])
        annotateTypesExpr env exprA
        declA.LocalVars <- env.LocalVars


let annotate (program:Program) =

    // Create an initial map of refs with all the functions return types
    let initRefs = Seq.map (fun (declA:DeclA) -> match declA.Item with
            | Proc (name, _, returnType, _) ->
                (name, Ref(name, returnType, Local))) program |> Map.ofSeq
                
    // Do a series of annotation passes
    Seq.iter (fun (d:DeclA) -> 
        // Add the initial refs first
        d.AddRefs(initRefs)
        annotateTypesDecl d) program

    // Return the annotated tree
    program