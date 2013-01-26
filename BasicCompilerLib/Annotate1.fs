module Annotate1

open Tree

(*

Blaze through the AST and get all names/locations of classes/interfaces, their methods and variables
and make a default map of references that can be added for everything

*)

let concatMap f items=
    Seq.map f items
    |> Seq.concat

let fst3 (a,b,c) = a
let snd3 (a,b,c) = b
let thd3 (a,b,c) = c

let getStdRefsClass namespace_ cname (cA:ClassDeclA) =
    let qname = qualifiedName namespace_ cname [cA.Name]

    match cA.Item with
        | ClassVar (name, vis, isStatic, ptype, eA) ->
            cA.PType <- ptype
            (vis, RefA(name, ClassLevelRef (qname, cA)))
        | ClassProc (name, vis, isStatic, isCtor, params_, returnType, eA) ->
            let ptype = paramsReturnTypeToPtype params_ returnType
            cA.PType <- ptype
            (vis, RefA(name, ClassLevelRef (qname, cA)))


let getStdRefsInterface namespace_ iname (iA:InterfaceDeclA) =
    match iA.Item with
        | InterfaceProc (name, params_, returnType) ->
            let qname = qualifiedName namespace_ iname [name]
            let ptype = paramsReturnTypeToPtype params_ returnType
            iA.PType <- ptype
            RefA(name, InterfaceLevelRef (qname, iA))


let getStdRefsNamespace namespace_ (nA:NamespaceDeclA) =
    let qname = qualifiedName namespace_ nA.Name []

    let (cr, laterActions) = match nA.Item with
        | Class (name, vis, cdeclAs) ->
            let crefs = Seq.map (getStdRefsClass namespace_ name) cdeclAs
            let filter vis = Seq.map snd <| Seq.filter (fun cref -> fst cref = Public) crefs
            let publicRefs = filter Public
            let privateRefs = filter Private
            
            let laterActions = [
                // Add public refs to class decl and lower
                (fun () -> iterAST foldASTNamespaceDecl (fun annot -> annot.AddRefs(publicRefs)) nA);
                // Add private refs only to those below class decl
                (fun () -> Seq.iter (iterAST foldASTClassDecl (fun annot -> annot.AddRefs(privateRefs))) cdeclAs)
            ]
            (NamespaceLevelRef (qname, nA), laterActions)
        | Interface (name, vis, ideclAs) ->
            let irefs = Seq.map (getStdRefsInterface namespace_ name) ideclAs
            // Copy down all refs
            let laterActions = [fun () -> iterAST foldASTNamespaceDecl (fun annot -> annot.AddRefs(irefs)) nA]
            (NamespaceLevelRef (qname, nA), laterActions)

    let globalRef = RefA(qname, cr)
    let localRef = RefA(nA.Name, cr)

    (globalRef, localRef, laterActions)


let getStdRefsTop (namespaceA:TopDeclA) =
    match namespaceA.Item with
        | Namespace (name, nAs) -> 
            let globalLocalRefs = Seq.map (getStdRefsNamespace name) nAs
            let globalRefs = Seq.map fst3 globalLocalRefs
            let localRefs = Seq.map snd3 globalLocalRefs
            let laterActions = concatMap thd3 globalLocalRefs

            // Add each of the local refs to the namespace and each local ref to the decls in the namespace
            let laterActions = Seq.append [fun () -> iterAST foldASTTopDecl (fun annot -> annot.AddRefs(localRefs)) namespaceA] laterActions

            (globalRefs, laterActions)
    

let getStdRefsCU (cu:CompilationUnit) = 
    // Find all the usings and collect them together
    let usings = Seq.fold (fun usingList (declA:TopDeclA) -> match declA.Item with
        | Using str -> str :: usingList
        | _ -> usingList) [] cu

    let namespaces = Seq.filter (fun (tdA:TopDeclA) -> tdA.IsNamespace) cu

    let topRefs = Seq.map getStdRefsTop namespaces
    let globalRefs = concatMap fst topRefs
    let laterActions = concatMap snd topRefs

    (globalRefs, laterActions)


let getStdRefs (program:Program) =
    let cuRefs = Seq.map getStdRefsCU program
    let globals = concatMap fst cuRefs
    let laterActions = concatMap snd cuRefs

    // Add each of the globals to all the things in the AST
    iterAST foldASTProgram (fun annot -> annot.AddRefs(globals)) program |> ignore

    // Now add all the other things we have stored up
    Seq.iter (fun act -> act ()) laterActions