module Annotate1

open Tree

(*

Blaze through the AST and get all names/locations of classes/interfaces, their methods and variables
and make a default map of references that can be added for everything

*)

let concatMap f items =
    Seq.map f items
    |> Seq.concat

let getStdRefsClass namespace_ cname (cA:ClassDeclA) =
    let qname = qualifiedName namespace_ cname [cA.Name]

    match cA.Item with
        | ClassVar (name, vis, isStatic, ptype, eA) ->
            cA.PType <- ptype
            cA.QName <- qname
            (name, ClassRef (name, cA))
        | ClassProc (name, vis, isStatic, isCtor, params_, returnType, eA) ->
            cA.PType <- paramsReturnTypeToPtype params_ returnType
            cA.QName <- qname
            (name, ClassRef (name, cA))


let getStdRefsInterface namespace_ iname (iA:InterfaceDeclA) =
    match iA.Item with
        | InterfaceProc (name, params_, returnType) ->
            iA.PType <- paramsReturnTypeToPtype params_ returnType
            iA.QName <- qualifiedName namespace_ iname [name]
            (name, InterfaceRef (name, iA))


let getStdRefsNamespace namespace_ (nA:NamespaceDeclA) =
    let qname = qualifiedName namespace_ nA.Name []

    // We need to go deeper - add CIRefs for classes/interfaces
    let refs = match nA.Item with
        | Class (name, vis, cAs) ->
            Seq.map (getStdRefsClass namespace_ name) cAs
        | Interface (name, vis, iAs) ->
            Seq.map (getStdRefsInterface namespace_ name) iAs

    nA.AddRefs(refs)
    (qname, nA)


let getStdRefsTop (tA:TopDeclA) =
    match tA.Item with
        | Namespace (name, nAs) ->
            // Add namespace to all subthings
            iterAST foldASTTopDecl (fun (annot:Annot) -> annot.Namespace <- name) tA
            Seq.map (getStdRefsNamespace name) nAs
    

let getStdRefsCU (cu:CompilationUnit) =
    // Find all the usings and collect them together
    let usings = Seq.fold (fun usingList (declA:TopDeclA) -> match declA.Item with
        | Using str -> str :: usingList
        | _ -> usingList) [] cu

    // Add usings to all subthings
    iterAST foldASTCompilationUnit (fun (annot:Annot) -> annot.Usings <- usings) cu
    |> ignore

    let namespaces = Seq.filter (fun (tdA:TopDeclA) -> tdA.IsNamespace) cu
    concatMap getStdRefsTop namespaces


let getStdRefs (program:Program) =
    let globals = concatMap getStdRefsCU program
    
    // Now make them into a map
    Map.ofSeq globals
