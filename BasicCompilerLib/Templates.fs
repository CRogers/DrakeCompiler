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



open Util

// Find template invocations, then try and expand the templates from that
// Template invocations can come about from: VarStatic, DotTemplate, ClassVar ptype,
//      Class/InterfaceProc params/returntypes, Interface Inheritance, Type Constraints
// Do a DFS on types - start off with the current set and each time a template is invokes
//      add it to the search. Stop when none are left to visit.

let copyOverTemplate (template:ITemplate) (new_:ITemplate) env =
    new_.TypeEnv <- env
    new_.Template <- Some template

let copyOverAnnot (template:Annot) (new_:Annot) =
    new_.Namespace <- template.Namespace
    new_.Usings <- template.Usings

let filterOutTemplates(items: list<#ITemplate>) =
    List.filter (not << isNonExpandedTemplate) items

let getTypeParam env name = match Map.tryFind name env with
    | Some v -> v
    | None -> failwithf "Could not find type param for %s" name



let paramedName name typeParams = sprintf "%s%s" name <| if Seq.length typeParams > 0 then sprintf "[%s]" <| Util.joinMap "," (fun (Type nA) -> nA.QName) typeParams else ""

let paramedNameEnv name template typeParamEnv = paramedName name <| Seq.map (fun tpn -> getTypeParam typeParamEnv tpn) (template :> ITemplate).TypeParams

let rec expandTemplatePType found env ptype = match ptype with
    | TypeParam p -> getTypeParam env p
    | Type nA -> ptype
    | ParamedType (Type templateNA, typeParams) ->
        //ParamedType (ptype, List.map (expandTemplatePType env) typeParams)
        let expandedParams = List.map (expandTemplatePType found env) typeParams
        let key = paramedName templateNA.QName expandedParams
        match Map.tryFind key !found with
            | Some nA -> Type nA
            | None -> failwithf "Type %s not found in template ptype expansion stage!" key

let expandTemplatePTypes found env ptypes = List.map (expandTemplatePType found env) ptypes

let expandTemplateParams found env params_ = List.map (fun (p:Param) -> Param(p.Name, expandTemplatePType found env p.PType)) params_
    
/////////
let rec expandTemplateExpr found env (templateEA:ExprA) =
    let eTE = expandTemplateExpr found env
    let eTEr eRef = ref <| eTE !eRef
    let eTP = expandTemplatePType found env
    let eTPr tRef = ref <| eTP !tRef
    let newE = match templateEA.Item with
        | ConstInt (size, v) -> ConstInt (size, v)
        | ConstBool b -> ConstBool b
        | Var s -> Var s
        | VarStatic ptype -> VarStatic <| eTPr ptype
        | Dot (eA, n) -> Dot (eTE eA, n)
        | DotTemplate (eA, n, typeParams) -> DotTemplate (eTE eA, n, ref (expandTemplatePTypes found env !typeParams))
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


/////////
let expandTemplateC found env (templateCA:CDA) =
    let newC = match templateCA.Item with
        | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
            let newName = paramedNameEnv name templateCA env
            let newParams = expandTemplateParams found env !params_
            let newReturnType = expandTemplatePType found env !returnType
            let newEA = expandTemplateExpr found env eA

            ClassProc (newName, vis, isStatic, ref newParams, ref newReturnType, newEA)
            
        | ClassVar (name, vis, isStatic, ptype, eA) ->
            let newPtype = expandTemplatePType found env !ptype
            let newEA = expandTemplateExpr found env eA

            ClassVar (name, vis, isStatic, ref newPtype, newEA)

    let newCA = ClassDeclA(newC, templateCA.Pos)
    copyOverTemplate templateCA newCA env
    copyOverAnnot templateCA newCA

    newCA.IsBinop <- templateCA.IsBinop

    newCA

/////////
let expandTemplateI found env (templateIA:IDA) =
    let newI = match templateIA.Item with
        | InterfaceProc (name, params_, returnType) ->
            let newName = paramedNameEnv name templateIA env
            let newParams = expandTemplateParams found env params_
            let newReturnType = expandTemplatePType found env !returnType

            InterfaceProc (newName, newParams, ref newReturnType)


    let newIA = InterfaceDeclA(newI, templateIA.Pos)
    copyOverTemplate templateIA newIA env
    copyOverAnnot templateIA newIA

    newIA

/////////
let expandTemplateN found env (newNA:NDA) (templateNA:NDA) =
    let toAnnotList items = List.map (fun cA -> cA :> Annot) items

    let newName, newN = match templateNA.Item with
        | Class (name, vis, isStruct, ifaces, cAs) ->
            let newName = paramedNameEnv name templateNA env
            let newIfaces = expandTemplatePTypes found env !ifaces
            let newCAs =
                filterOutTemplates cAs
                |> List.map (expandTemplateC found env)
                
            newName, Class (newName, vis, isStruct, ref newIfaces, newCAs)

        | Interface (name, vis, ifaces, iAs) ->
            let newName = paramedNameEnv name templateNA env
            let newIfaces = expandTemplatePTypes found env !ifaces
            let newIAs =
                filterOutTemplates iAs
                |> List.map (expandTemplateI found env)
                
            newName, Interface (newName, vis, ref newIfaces, newIAs)

    newNA.Item <- newN
        
    // Add Namespace/Usings, NamespaceDecl CIRefs
    copyOverTemplate templateNA newNA env
    copyOverAnnot templateNA newNA

    iterAST foldASTNamespaceDecl (fun annot -> annot.NamespaceDecl <- Some newNA) newNA

    newNA
        


/////////
let rec findTemplateExpansionsPType found ptype = match ptype with
    | TypeParam p -> failwithf "Shoudln't be finding TypeParams here"
    | Type nA -> ptype
    | ParamedType (Type templateNA, typeParams) ->
        let expandedTypeParams:list<PType> = findTemplateExpansionsPTypes found typeParams

        let key = paramedName templateNA.QName expandedTypeParams

        // See if type does not already exists
        match Map.tryFind key !found with
            | None ->
                let templateTPs = (templateNA :> ITemplate).TypeParams

                // Check that the type we're parameterising has the right number of type params
                if not (expandedTypeParams.Length = templateTPs.Length) then
                    failwithf "Parameterisation of type %s has too many parameters: %s" templateNA.QName key

                let env = Map.ofSeq <| Seq.zip templateTPs expandedTypeParams

                // Create a placeholder concrete template type so during the expansion phase it can reference itself
                let newNA = NamespaceDeclA(Interface("",Public,ref [],[]), Pos.NilPos)
                found := Map.add key newNA !found
                               
                // Expand it out properly
                expandTemplateN found env newNA templateNA |> ignore           

                // Add it to the search
                findTemplateExpansionsNamespace found newNA

                Type newNA
            | Some nA ->
                Type nA
    
and findTemplateExpansionsPTypes found ptypes = List.map (findTemplateExpansionsPType found) ptypes

and findTemplateExpansionsParams found params_ = List.map (fun (p:Param) -> findTemplateExpansionsPType found p.PType) params_

////////
and findTemplateExpansionsExpr found (templateEA:ExprA) =
    let fTE = findTemplateExpansionsExpr found
    let fTEr eRef = fTE !eRef
    let fTP = findTemplateExpansionsPType found
    let fTPr tRef = fTP !tRef
    match templateEA.Item with
        | ConstInt _
        | ConstBool _ 
        | Var _ -> ()
        | VarStatic ptype -> ptype := fTPr ptype
        | Dot (eA, _) -> fTE eA
        | DotTemplate (eA, _, typeParams) -> fTE eA; typeParams := findTemplateExpansionsPTypes found !typeParams
        | Binop (_, leA, reA) -> fTE leA; fTE reA
        | Cast (ptype, eA) -> ptype := fTPr ptype; fTE eA
        | Call (eA, argEAs) -> fTE eA; Seq.iter fTE argEAs
        | Assign (eA1, eA2) -> fTE eA1; fTE eA2
        | DeclVar (_, eA) -> fTE eA
        | Return eA -> fTE eA
        | ReturnVoid -> ()
        | If (test_, then_, else_) -> fTE test_; fTE then_; fTE else_
        | While (test, body) -> fTE test; fTE body
        | Seq (e1, e2) -> fTEr e1; fTEr e2
        | Nop -> ()
        | _ -> failwithf "Unknown case when template expanding expr"

/////////
and findTemplateExpansionsClass found (templateCA:CDA) =
    match templateCA.Item with
        | ClassVar (_, _, _, ptype, eA) ->
            findTemplateExpansionsPType found !ptype |> ignore
            findTemplateExpansionsExpr found eA
        | ClassProc (_, _, _, params_, returnType, eA) ->
            findTemplateExpansionsParams found !params_ |> ignore
            findTemplateExpansionsPType found !returnType |> ignore
            findTemplateExpansionsExpr found eA

/////////
and findTemplateExpansionsInterface found (templateIA:IDA) =
    match templateIA.Item with
        | InterfaceProc (_, params_, returnType) ->
            findTemplateExpansionsParams found params_ |> ignore
            findTemplateExpansionsPType found !returnType |> ignore

/////////
and findTemplateExpansionsNamespace found (templateNA:NDA) =
    match templateNA.Item with
        | Class (_, _, _, ifaces, cAs) ->
            findTemplateExpansionsPTypes found !ifaces |> ignore
            Seq.iter (findTemplateExpansionsClass found) cAs

        | Interface (_, _, ifaces, iAs) ->
            findTemplateExpansionsPTypes found !ifaces |> ignore
            Seq.iter (findTemplateExpansionsInterface found) iAs
    
let expandTemplates (globals:GlobalStoreRef) (program:list<NDA>) =
    // Filter out the original, non-expanded templates
    let nonTemplates = filterOutTemplates program

    // Start off with the non-templates being "found"
    let found = ref << Map.ofSeq <| seq { for nA in nonTemplates do yield (nA.QName, nA) }
    
    // Search each of them
    Seq.iter (findTemplateExpansionsNamespace found) nonTemplates

    // Change the Globals
    globals := !found

    // Return the expanded templates
    List.ofSeq <| Seq.map snd (Map.toSeq !found)


