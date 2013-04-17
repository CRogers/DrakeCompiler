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

// Find template invocations, then try and expand the templates from that
