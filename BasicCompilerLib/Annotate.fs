﻿module Annotate

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
        | ConstInt (size, _) -> commonPtype <| Int size
        | ConstBool _        -> commonPtype Bool
        | ConstUnit          -> commonPtype Unit
        | Var n              -> exprA.GetRef(n).PType
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
            // Check to see if call works/what result is
            let exprAsPtypes = List.map (fun (exprA:ExprA) -> exprA.PType) exprAs
            let funcPtype = exprA.GlobalDecls.GetNamespaceDecl(exprA.Namespace, exprA.UsingContext, name)
            match funcPtype with
                | PFunc (argTypes, returnType) ->
                    if not (argTypes = exprAsPtypes) then
                        failwithf "Incorrect function args"
                    returnType
                | _ -> failwithf "Can only call a function!"
            
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

let annotateClassDecl (declA:ClassDeclA) = match declA.Item with
    | ClassProc (name, vis, params_, returnType, exprA) ->
        // Add the parameters to the body's environment
        Seq.iter (fun (p:Param) -> declA.AddRef(Ref(p.Name, p.PType, Parameter))) params_
        let env = Env([])

        // Get the decl's type from the initRefs
        declA.PType <- declA.GetRef(name).PType

        // Add the refs from the decl to it's subexpression
        annotateTypesExpr env declA.Refs exprA
        declA.LocalVars <- env.LocalVars

let annotateInterfaceDecl (ideclA:InterfaceDeclA) = match ideclA.Item with
    | InterfaceProc (name, params_, returnType) ->
        let ptype = paramsReturnTypeToPtype params_ returnType
        ideclA.PType <- ptype

let annotateNamespaceDecl (ndeclA:NamespaceDeclA) =
    // Add things to the global store if they are public
    let addGlobalIfPublic name vis =
        if vis = Public then
            ndeclA.GlobalDecls.AddNamespaceDecl(ndeclA)

    match ndeclA.Item with
        | Class (name, vis, cdeclAs) ->
            addGlobalIfPublic name vis
            List.iter annotateClassDecl cdeclAs
        | Interface (name, vis, ideclAs) ->
            addGlobalIfPublic name vis
            List.iter annotateInterfaceDecl ideclAs


let annotateNamespace (namespace_:TopDeclA) = match namespace_.Item with
    | Namespace (name, ndeclAs) ->
        // Add the namespace name to all subthings
        iterAST foldASTTopDecl (fun itemA -> itemA.Namespace <- name) namespace_ |> ignore
        // Annotate all subthings
        Seq.iter annotateNamespaceDecl ndeclAs
    |  _ -> failwithf "Expecting namespace"

let annotateCompilationUnit globals (cu:CompilationUnit) =
    // Find all the usings and collect them together
    let usings = Seq.fold (fun usingList (declA:TopDeclA) -> match declA.Item with
        | Using str -> str :: usingList
        | _ -> usingList) [] cu

    // Add usings to namespace contexts
    let namespaces = (Seq.filter (fun (declA:TopDeclA) -> match declA.Item with
        | Namespace (name, decls) ->
            // Add the UsingContext to all subobjects
            iterAST foldASTTopDecl (fun itemA -> itemA.UsingContext <- usings) declA |> ignore
            true
        | _ -> false)) cu

    Seq.iter annotateNamespace namespaces

let annotate (program:Program) =

    

    // annotate each compilation unit
    Seq.iter (annotateCompilationUnit) program

    // Create an initial map of refs with all the functions return types
    let initRefs = Seq.map (fun (declA:ClassDeclA) -> match declA.Item with
            | ClassProc (name, vis, params_, returnType, _) ->
                let ptype = paramsReturnTypeToPtype params_ returnType
                (name, Ref(name, ptype, Local))) program |> Map.ofSeq
                
    // Do a series of annotation passes
    Seq.iter (fun (d:DeclA) -> 
        // Add the initial refs first
        d.AddRefs(initRefs)
        annotateTypesDecl d) program

    // Return the annotated tree
    program