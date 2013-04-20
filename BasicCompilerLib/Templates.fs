module Templates

open Tree

// Go through InitialTypes and change them to TypeParams when necessary
let annotateTypeParams (program:list<NDA>) =

    /////////
    let checkNoRepeatedTypeParams typeParamNames =
        let dups = Util.findDuplicates Util.id typeParamNames
        if Seq.length dups > 0 then
            failwithf "Type params %s are repeated multiple times" <| Util.joinMap ", " List.head dups
            

    /////////
    let checkNotTypeParam typeParams ptype = match ptype with
        | InitialType name ->
            if Set.contains name typeParams then
                failwithf "%s is a type param when it shouldn't be" name
        

    /////////
    let rec newPType typeParams (ptype:PType) =
        match ptype with
            | InitialType name ->
                if Set.contains name typeParams then
                    TypeParam name
                else
                    ptype
            | ParamedType (pdtype, params_) ->
                // The LHS of the Type~[T1,..,TN] expression must be a concrete type and not a type param
                checkNotTypeParam typeParams pdtype
                ParamedType (pdtype, newPTypes typeParams params_)
    
    and newPTypes typeParams ptypes = List.map (newPType typeParams) ptypes

    /////////
    let expandPTypesParams typeParams (params_:list<Param>) = 
        for p in params_ do
            p.PType <- newPType typeParams p.PType
    
    /////////
    let aTPExpr typeParams (eA:ExprA) =
        // The exprs that have type information in them are: Cast, VarStatic, DotTemplate
        let change (annot:Annot) =
            let eA = annot :?> ExprA
            match eA.Item with
                | Cast (ptype, _) -> ptype := newPType typeParams !ptype
                | VarStatic ptype -> ptype := newPType typeParams !ptype
                | DotTemplate (_, _, ptypes) -> ptypes := newPTypes typeParams !ptypes
                | _ -> ()

        iterAST foldASTExpr change eA

    ////////
    let aTPInterface typeParams (iA:IDA) =
        match iA.Item with
            | InterfaceProc (_, params_, returnType) ->
                let typeParams = Set.union typeParams <| Set.ofList (iA :> ITemplate).TypeParams
                expandPTypesParams typeParams params_

    ////////
    let aTPClass typeParams (cA:CDA) =
        match cA.Item with
            | ClassVar (_, _, _, ptype, eA) ->
                ptype := newPType typeParams !ptype
                aTPExpr typeParams eA
            | ClassProc (_, _, _, params_, returnType, eA) ->
                let typeParams = Set.union typeParams <| Set.ofList (cA :> ITemplate).TypeParams
                expandPTypesParams typeParams !params_
                returnType := newPType typeParams !returnType
                aTPExpr typeParams eA

    ////////
    let aTPNamespace (nA:NDA) =
        let nAIT = nA :> ITemplate

        checkNoRepeatedTypeParams nAIT.TypeParams

        let typeParams = Set.ofList nAIT.TypeParams
        let typeConstraints = nAIT.TypeConstraints

        match nA.Item with
            | Class (_, _, _, ifaces, cAs) ->
                ifaces := newPTypes typeParams !ifaces
                Seq.iter (aTPClass typeParams) cAs
            | Interface (_, _, ifaces, iAs) ->
                ifaces := newPTypes typeParams !ifaces
                Seq.iter (aTPInterface typeParams) iAs

    Seq.iter aTPNamespace program


open System.Collections.Generic

// Find template invocations, then try and expand the templates from that
// Template invocations can come about from: VarStatic, DotTemplate, ClassVar ptype,
//      Class/InterfaceProc params/returntypes, Interface Inheritance, Type Constraints
// Do a DFS on types - start off with the current set and each time a template is invokes
//      add it to the search. Stop when none are left to visit.
let expandTemplates (program:list<NDA>) =
    
    let found = new Dictionary<string, NDA>()

    let printPType ptype = match ptype with
        | Type nA -> nA.QName
        | TypeParam s -> s

    let printPTypes ptypes = Util.joinMap ", " printPType ptypes

    let getTypeParam env name = match Map.tryFind name env with
        | Some v -> v
        | None -> failwithf "Could not find type param for %s" name 

    let paramTypeKey (nA:NDA) params_ = sprintf "%s[%s]" nA.QName <| Util.joinMap "," (fun (Type nA) -> nA.QName) params_

    let copyOverTemplate (template:ITemplate) (new_:ITemplate) env =
        new_.TypeParams <- template.TypeParams
        new_.TypeConstraints <- template.TypeConstraints
        new_.TypeEnv <- env

    let copyOverAnnot (template:Annot) (new_:Annot) namespaceDecl =
        new_.Namespace <- template.Namespace
        new_.NamespaceDecl <- namespaceDecl
        new_.Usings <- template.Usings

    let filterOutTemplates(items: list<#ITemplate>) =
        List.filter (isNonExpandedTemplate >> not) items
        
    // Take a ptype and replace it's TypeParams with real types, create and return a new type for it
    // If it's not a parameterised type, use return it
    let rec expandTemplatePType env ptype =
        match ptype with
            | TypeParam name -> getTypeParam env name
            | Type nA -> ptype
            | ParamedType (Type nA, ptparams) ->
                let typeParams = (nA :> ITemplate).TypeParams

                // Check that the type we're parameterising has the right number of type params
                if not (typeParams.Length = ptparams.Length) then
                    failwithf "Parameterisation of type %s has too many parameters: %s" nA.QName <| printPTypes ptparams

                // Recursively replace each type param with concrete versions
                let concretePtparams = seq {
                    for ptparam in ptparams do
                        yield expandTemplatePType env ptparam
                }

                // See if we've visited it before
                let newNA =
                    let key = paramTypeKey nA concretePtparams
                    if not <| found.ContainsKey key then
                        // Match up concrete type param with name (key) and expand out the new template and return it
                        let newEnv = Map.ofSeq <| Seq.zip typeParams concretePtparams
                        expandTemplateNamespace newEnv nA
                    else
                        // Or return the one we made already
                        found.[key]

                Type newNA

            | _ -> failwithf "Compiler Error: InitialType still exists at template expansion stage"

    and expandTemplatesPTypes env ptypes = List.map (expandTemplatePType env) ptypes

    and expandTemplatesParams env params_ = List.map (fun (p:Param) -> Param(p.Name, expandTemplatePType env p.PType)) params_

    and expandTemplateExpr env (templateEA:ExprA) =
        let eTE = expandTemplateExpr env
        let eTEr eRef = ref <| eTE !eRef
        let eTP = expandTemplatePType env
        let eTPr tRef = ref <| eTP !tRef
        let newE = match templateEA.Item with
            | ConstInt (size, v) -> ConstInt (size, v)
            | ConstBool b -> ConstBool b
            | Var s -> Var s
            | VarStatic ptype -> VarStatic <| eTPr ptype
            | Dot (eA, n) -> Dot (eTE eA, n)
            | DotTemplate (eA, n, typeParams) -> DotTemplate (eTE eA, n, ref (expandTemplatesPTypes env !typeParams))
            | Binop (n, leA, reA) -> Binop (n, eTE leA, eTE reA)
            | Cast (ptype, eA) -> Cast (eTPr ptype, eTE eA)
            | Call (eA, argEAs) -> Call (eTE eA, List.map eTE argEAs)
            | Assign (eA1, eA2) -> Assign (eTE eA1, eTE eA2)
            | DeclVar (n, eA) -> DeclVar (n, eTE eA)
            | Return eA -> Return <| eTE eA
            | ReturnVoid -> ReturnVoid
            | If (test_, then_, else_) -> If (eTE test_, eTE then_, eTE else_)
            | While (test, body) -> While (eTE test, eTE body)
            | Seq (e1, e2) -> Seq (eTEr e1, eTEr e2)
            | Nop -> Nop
            | _ -> failwithf "Unknown case when template expanding expr" 

        let newEA = ExprA(newE, templateEA.Pos)
        newEA

    and expandTemplateClass env ndecl (templateCA:CDA) =
        let newC = match templateCA.Item with
            | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
                let newParams = expandTemplatesParams env !params_
                let newReturnType = expandTemplatePType env !returnType

                ClassProc (name, vis, isStatic, ref newParams, ref newReturnType, eA)
            
            | ClassVar (name, vis, isStatic, ptype, eA) ->
                let newPtype = expandTemplatePType env !ptype

                ClassVar (name, vis, isStatic, ref newPtype, eA)

        let newCA = ClassDeclA(newC, templateCA.Pos)
        copyOverTemplate templateCA newCA env
        copyOverAnnot templateCA newCA ndecl

        newCA.IsBinop <- templateCA.IsBinop

        

    and expandTemplateInterface env ndecl (templateIA:IDA) =
        let newI = match templateIA.Item with
            | InterfaceProc (name, params_, returnType) ->
                let newParams = expandTemplatesParams env params_
                let newReturnType = expandTemplatePType env !returnType

                InterfaceProc (name, newParams, ref newReturnType)


        let newIA = InterfaceDeclA(newI, templateIA.Pos)
        copyOverTemplate templateIA newIA env
        copyOverAnnot templateIA newIA ndecl

        newIA


    // We take a template and make a new version of the class that has the type parameters changed
    // To begin with, only look at methods where there are no type parameters attached to them
    and expandTemplateNamespace env (templateNA:NDA) =
        let newN = match templateNA.Item with
            | Class (name, vis, isStruct, ifaces, cAs) ->
                let newIfaces = expandTemplatesPTypes env !ifaces
                let newCAs =
                    filterOutTemplates cAs
                    |> List.map (expandTemplateClass env None) 
                
                Class (name, vis, isStruct, ref newIfaces, [])

            | Interface (name, vis, ifaces, iAs) ->
                let newIfaces = expandTemplatesPTypes env !ifaces
                let newIAs =
                    filterOutTemplates iAs
                    |> List.map (expandTemplateInterface env)
                
                Interface (name, vis, ref newIfaces, newIAs)

        match newN with
            | None -> ()
            | Some nd -> 
                let newNA = NamespaceDeclA(newN, templateNA.Pos)
        
                // Add Namespace/Usings, NamespaceDecl CIRefs
                copyOverTemplate templateNA newNA env

                newNA.QName <- paramTypeKey templateNA 

                found.Add(newNA.QName, newNA)



        ()
    

    // Filter out the original, non-expanded templates
    let nonTemplates = filterOutTemplates program

    // Start the search from each of the non-template "roots"
    for nA in nonTemplates do
        expandTemplateNamespace Map.empty nA

    // Results are in found
    let expandedTemplates = found.Values

    // Return non-template class and expanded templates
    Seq.append nonTemplates expandedTemplates
    |> List.ofSeq

