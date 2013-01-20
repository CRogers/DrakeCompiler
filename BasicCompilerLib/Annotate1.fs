module Annotate1

open Tree

(*

Blaze through the AST and get all names/locations of classes/interfaces, their methods and variables
and make a default map of references that can be added for everything

*)

let concatMap f items=
    Seq.map f items
    |> Seq.concat


let getStdRefsClass namespace_ cname (cA:ClassDeclA) =
    let qname = qualifiedName namespace_ cname [cA.Name]

    match cA.Item with
        | ClassVar (name, vis, ptype, eA) ->
            (vis, RefA(name, LocalRef (qname, ptype)))
        | ClassProc (name, vis, params_, returnType, eA) ->
            let ptype = paramsReturnTypeToPtype params_ returnType
            (vis, RefA(name, LocalRef (qname, ptype)))


let getStdRefsInterface namespace_ iname (iA:InterfaceDeclA) =
    match iA.Item with
        | InterfaceProc (name, params_, returnType) ->
            let qname = qualifiedName namespace_ iname [name]
            let ptype = paramsReturnTypeToPtype params_ returnType
            RefA(name, LocalRef (qname, ptype))


let getStdRefsNamespace namespace_ (nA:NamespaceDeclA) =
    let qname = qualifiedName namespace_ nA.Name []

    let cr = match nA.Item with
        | Class (name, vis, cdeclAs) ->
            let crefs = Seq.map (getStdRefsClass namespace_ name) cdeclAs
            let filter vis = Seq.map snd <| Seq.filter (fun cref -> fst cref = Public) crefs
            let publicRefs = filter Public
            let privateRefs = filter Private
            // Add public refs to class decl and lower
            iterAST foldASTNamespaceDecl (fun annot -> annot.AddRefs(publicRefs)) nA
            // Add private refs only to those below class decl
            Seq.iter (iterAST foldASTClassDecl (fun annot -> annot.AddRefs(privateRefs))) cdeclAs
            ClassRef (qname, nA)
        | Interface (name, vis, ideclAs) ->
            let irefs = Seq.map (getStdRefsInterface namespace_ name) ideclAs
            // Copy down all refs
            iterAST foldASTNamespaceDecl (fun annot -> annot.AddRefs(irefs)) nA
            InterfaceRef (qname, nA)

    let globalRef = RefA(qname, cr)
    let localRef = RefA(nA.Name, cr)

    (globalRef, localRef)


let getStdRefsTop (namespaceA:TopDeclA) =
    match namespaceA.Item with
        | Namespace (name, nAs) -> 
            let globalLocalRefs = Seq.map (getStdRefsNamespace name) nAs
            let globalRefs = Seq.map fst globalLocalRefs
            let localRefs = Seq.map snd globalLocalRefs

            // Add each of the local refs to the namespace and each local ref to the decls in the namespace
            iterAST foldASTTopDecl (fun annot -> annot.AddRefs(localRefs)) namespaceA

            globalRefs
    

let getStdRefsCU (cu:CompilationUnit) = 
    // Find all the usings and collect them together
    let usings = Seq.fold (fun usingList (declA:TopDeclA) -> match declA.Item with
        | Using str -> str :: usingList
        | _ -> usingList) [] cu

    let namespaces = Seq.filter (fun (tdA:TopDeclA) -> tdA.IsNamespace) cu

    concatMap getStdRefsTop namespaces


let getStdRefs (program:Program) =
    let globals = concatMap getStdRefsCU program

    // Add each of the globals to all the things in the AST
    iterAST foldASTProgram (fun annot -> annot.AddRefs(globals)) program