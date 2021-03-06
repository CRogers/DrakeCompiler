﻿module AnnotateRefs

open Tree
open Util

(*

Blaze through the AST and get all names/locations of classes/interfaces, their methods and variables
and make a default map of references that can be added for everything

*)           


// Go through an expand all types that are shorthand (eg Int32 instead of System::Int32) and make them longhand
let expandTypes (globals:GlobalStore) =

    //////////
    let getQName namespace_ usings name =
        // See if it is a qualified name first
        if isQualifiedName name then
            // Look up in globals
            match Map.tryFind name globals with
                | Some nA -> nA
                | None -> failwithf "Couldn't find %s in globals" name
        else
            // Look in local place first then usings
            let placesToLook = Seq.append [namespace_] usings

            let lookInNamespace nspace =
                Map.tryPick (fun key value ->
                    if key = qualifiedName nspace name [] then Some value
                    else None) globals

            match Seq.tryPick lookInNamespace placesToLook with
                | Some nA -> nA
                | None -> failwithf "Couldn't find %s in globals" name           

    //////////
    let rec newPType nspace usings ptype = match ptype with
            | Undef -> Undef
            | InitialType name -> Type <| getQName nspace usings name
            | TypeParam _ -> ptype
            | ParamedType (ptype, params_) -> ParamedType (newPType nspace usings ptype, newPTypes nspace usings params_)
            | Type nA -> failwithf "%s is trying to be expanded - this shouldn't happen at this stage" nA.Name

    and newPTypes nspace usings ptypes = List.map (newPType nspace usings) ptypes

    //////////
    let expandParamsQName nspace usings (params_:seq<Param>) =
        for p in params_ do
            p.PType <- newPType nspace usings p.PType

    //////////
    let expandTypesExpr (eA:ExprA) =
        
        let change (annot:Annot) =
            let eA = annot :?> ExprA
            match eA.Item with
                | Cast (ptype, _) -> ptype := newPType eA.Namespace eA.Usings !ptype
                | VarStatic ptype -> ptype := newPType eA.Namespace eA.Usings !ptype
                | VarTemplate (_, ptypes) -> ptypes := newPTypes eA.Namespace eA.Usings !ptypes
                | DotTemplate (_, _, ptypes) -> ptypes := newPTypes eA.Namespace eA.Usings !ptypes
                | _ -> ()

        iterAST foldASTExpr change eA

    //////////
    let expandTypesClass (cA:CDA) =
        let qname = cA.NamespaceDecl.Value.QName + "." + cA.Name

        let eA = match cA.Item with
            | ClassVar (name, vis, isStatic, ptype, eA) ->
                ptype := newPType cA.Namespace cA.Usings !ptype
                eA
            | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
                expandParamsQName cA.Namespace cA.Usings !params_
                returnType := newPType cA.Namespace cA.Usings !returnType
                eA

        expandTypesExpr eA

    //////////
    let expandTypesInterface iname (iA:IDA) =
        match iA.Item with
            | InterfaceProc (name, params_, returnType) ->
                expandParamsQName iA.Namespace iA.Usings params_
                returnType := newPType iA.Namespace iA.Usings !returnType

    //////////
    let expandTypesNamespace (nA:NDA) = 
        match nA.Item with
            | Class (name, vis, isStruct, ifaces, cAs) ->
                ifaces := List.map (newPType nA.Namespace nA.Usings) !ifaces
                Seq.iter expandTypesClass cAs
            | Interface (name, vis, ifaces, iAs) ->
                ifaces := List.map (newPType nA.Namespace nA.Usings) !ifaces
                Seq.iter (expandTypesInterface name) iAs


    Seq.iter expandTypesNamespace <| globalsToNAs globals


// Blaze through the AST and get the names of all the main types
let getGlobalRefs (program:Program) =

    //////////
    let getGlobalRefsNamespace namespace_ (nA:NamespaceDeclA) =
        // Identify sub things with which class they are in
        iterAST foldASTNamespaceDecl (fun (annot:Annot) -> annot.NamespaceDecl <- Some nA) nA |> ignore
        (nA.QName, nA)

    //////////
    let getGlobalRefsTop (tA:TopDeclA) =
        match tA.Item with
            | Namespace (name, nAs) ->
                // Add namespace to all subthings
                iterAST foldASTTopDecl (fun (annot:Annot) -> annot.Namespace <- name) tA
                Seq.map (getGlobalRefsNamespace name) nAs
    
    //////////
    let getGlobalRefsCU (cu:CompilationUnit) =
        // Find all the usings and collect them together
        let usings = Seq.fold (fun usingList (declA:TopDeclA) -> match declA.Item with
            | Using str -> str :: usingList
            | _ -> usingList) [] cu

        // Add usings to all subthings
        iterAST foldASTCompilationUnit (fun (annot:Annot) -> annot.Usings <- usings) cu
        |> ignore

        let namespaces = Seq.filter (fun (tdA:TopDeclA) -> tdA.IsNamespace) cu
        concatMap getGlobalRefsTop namespaces


    let globals = concatMap getGlobalRefsCU program

    // Now make them into a map
    ref <| Map.ofSeq globals
