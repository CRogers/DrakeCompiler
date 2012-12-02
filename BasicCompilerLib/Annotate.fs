module Annotate

open System.Collections.Generic
open Print
open Tree
open Builtins

let rec annotateTypesExpr (exprA:ExprA) =    
    exprA.PType <- match exprA.Item with
        | ConstInt _ ->  Int
        | ConstBool _ -> Bool
        | Var n -> exprA.GetRef(n).PType
        | Binop (op, l, r) ->
            annotateTypesExpr l
            annotateTypesExpr r
            let type_ = binopToType op l.PType r.PType
            if type_ = None then
                failwithf "Incorrect type for binary operation:\n%s" (fmt exprA.Item)
            else
                type_.Value
        | Call (name, exprAs) ->
            Seq.iter (annotateTypesExpr) exprAs
            exprA.GetRef(name).PType
        | Assign (name, innerExprA) ->
            annotateTypesExpr innerExprA
            innerExprA.PType
        | Print exprA ->
            annotateTypesExpr exprA
            exprA.PType
        | DeclVar (name, assignA) ->
            annotateTypesExpr assignA
            // Since we have declared a variable, add a reference object for it
            exprA.AddRef(Ref(name, assignA.PType, Local))
            assignA.PType
        | Return exprA ->
            annotateTypesExpr exprA
            Unit
        | If (test, then_, else_) ->
            annotateTypesExpr test
            annotateTypesExpr then_
            annotateTypesExpr else_
            then_.PType
        | While (exprA, stmtAs) ->
            annotateTypesExpr exprA
            annotateTypesExpr stmtAs
            stmtAs.PType
        | Seq (e1A, e2A) ->
            annotateTypesExpr e1A
            // Since e2A is lexically below and and in the same scope as e1A, all 
            // e1A's references also appear in e2A
            e2A.AddRefs(e1A.Refs)
            annotateTypesExpr e2A
            e2A.PType

let annotateTypesDecl (declA:DeclA) = match declA.Item with
    | Proc (name, params_, returnType, exprA) ->
        // Add the parameters to the body's environment
        Seq.iter (fun (p:Param) -> exprA.AddRef(Ref(p.Name, p.PType, Parameter))) params_
        annotateTypesExpr exprA


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