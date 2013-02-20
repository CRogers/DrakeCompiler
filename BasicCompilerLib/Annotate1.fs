module Annotate1

open Tree

(*

Blaze through the AST and get all names/locations of classes/interfaces, their methods and variables
and make a default map of references that can be added for everything

*)

let concatMap f items =
    Seq.map f items
    |> Seq.concat

let annotateCIRefs (globals:GlobalStore) (program:seq<NamespaceDeclA>) =

    let getQName namespace_ usings name =
        // See if it is a qualified name first
        if isQualifiedName name then
            // Look up in globals
            match Map.tryFindKey (fun key value -> key = name) globals with
                | None -> failwithf "Couldn't find %s in globals" name
                | Some x -> x
        else
            // Look in local place first then usings
            let placesToLook = Seq.append [namespace_] usings
            match Seq.tryPick (fun nspace -> Map.tryFindKey (fun key value -> key = qualifiedName nspace name []) globals) placesToLook with
                | Some qname -> qname
                | None -> match Map.tryFindKey (fun key value -> key = name) globals with
                    | None -> failwithf "Couldn't find %s in globals" name
                    | Some x -> x                  


    let rec newPType nspace usings ptype = match ptype with
            | Undef -> Undef
            | UserType name -> UserType <| getQName nspace usings name
            | PFunc (args, ret) -> PFunc (List.map (newPType nspace usings) args, newPType nspace usings ret)

    let expandParamsQName nspace usings (params_:seq<Param>) =
        Seq.iter (fun (p:Param) -> p.PType <- newPType nspace usings p.PType) params_


    let annotateCIRefsClass cname (cA:ClassDeclA) =
        let qname = qualifiedName cA.Namespace cname [cA.Name]

        match cA.Item with
            | ClassVar (name, vis, isStatic, ptype, eA) ->
                ptype := newPType cA.Namespace cA.Usings !ptype
                cA.QName <- qname
                (name, ClassRef cA)
            | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
                // Check to see that proc name isn't the same as the classname
                if name = cname then failwithf "Can't use %s as the name for class %s - must be different" name name
                // Type expansion
                expandParamsQName cA.Namespace cA.Usings params_
                returnType := paramsReturnTypeToPtype params_ returnType
                cA.QName <- qname
                (name, ClassRef cA)


    let annotateCIRefsInterface iname (iA:InterfaceDeclA) =
        match iA.Item with
            | InterfaceProc (name, params_, returnType) ->
                expandParamsQName iA.Namespace iA.Usings params_
                returnType := paramsReturnTypeToPtype params_ returnType
                iA.QName <- qualifiedName iA.Namespace iname [name]
                (name, InterfaceRef iA)


    let annotateCIRefsNamespace (nA:NamespaceDeclA) =
        // We need to go deeper - add CIRefs for classes/interfaces
        let refs = match nA.Item with
            | Class (name, vis, cAs) ->
                nA.QName <- nA.Namespace + "::" + name
                Seq.map (annotateCIRefsClass name) cAs
            | Interface (name, vis, iAs) ->
                nA.QName <- nA.Namespace + "::" + name
                Seq.map (annotateCIRefsInterface name) iAs

        nA.AddRefs(refs)


    Seq.iter annotateCIRefsNamespace program




let getGlobalRefs (program:Program) =

    let getGlobalRefsNamespace namespace_ (nA:NamespaceDeclA) =
        let qname = qualifiedName namespace_ nA.Name []
        (qname, nA)


    let getGlobalRefsTop (tA:TopDeclA) =
        match tA.Item with
            | Namespace (name, nAs) ->
                // Add namespace to all subthings
                iterAST foldASTTopDecl (fun (annot:Annot) -> annot.Namespace <- name) tA
                Seq.map (getGlobalRefsNamespace name) nAs
    

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
    Map.ofSeq globals
