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

let rec annotateTypesExpr (env:Env) prevRefs (exprA:ExprA) =
    exprA.AddRefs(prevRefs)
    let aTE env eA = annotateTypesExpr env exprA.Refs eA
    exprA.PType <- match exprA.Item with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | ConstUnit -> Unit
        | Var n -> exprA.GetRef(n).PType
        | Binop (op, l, r) ->
            aTE env l
            aTE env r
            let type_ = binopToType op l.PType r.PType
            if type_ = None then
                failwithf "Incorrect type for binary operation:\n%s" (fmt exprA.Item)
            else
                type_.Value
        | Call (name, exprAs) ->
            Seq.iter (aTE env) exprAs
            exprA.GetRef(name).PType
        | Assign (name, innerExprA) ->
            aTE env innerExprA
            innerExprA.PType
        | DeclVar (name, assignA) ->
            aTE env assignA
            // Since we have declared a variable, add a reference object for it
            let ref = Ref(name, assignA.PType, Local)
            env.AddLocalVar(ref)
            exprA.AddRef(ref)
            assignA.AddRef(ref)
            assignA.PType
        | Print exprA ->
            aTE env exprA
            exprA.PType
        | Return exprA ->
            aTE env exprA
            exprA.PType
        | If (test, then_, else_) ->
            aTE env test
            aTE env then_
            aTE env else_
            then_.PType
        | While (exprA, stmtAs) ->
            aTE env exprA
            aTE env stmtAs
            stmtAs.PType
        | Seq (e1A, e2A) ->
            aTE env e1A
            // Since e2A is lexically below and and in the same scope as e1A, all 
            // e1A's references also appear in e2A
            annotateTypesExpr env e1A.Refs e2A
            e2A.PType

let annotateTypesDecl (declA:DeclA) = match declA.Item with
    | Proc (name, params_, returnType, exprA) ->
        // Add the parameters to the body's environment
        Seq.iter (fun (p:Param) -> declA.AddRef(Ref(p.Name, p.PType, Parameter))) params_
        let env = Env([])
        // Add the refs from the decl to it's subexpression
        annotateTypesExpr env declA.Refs exprA
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