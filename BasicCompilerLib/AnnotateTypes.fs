module AnnotateTypes

open Tree
open Builtins
open Print
open Util
open Polymorphism
open System.Collections.Generic

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let annotateTypes (globals:GlobalStoreRef) (binops:BinopStore) =

    /////////////
    let lowerBinopToCall (eA:ExprA) = match eA.Item with
        | Binop (n, l, r) ->
            let argTypeNAs = List.map (fun (eA:ExprA) -> ptypeToNA eA.PType) [l; r]
            let cA = match getBestOverload "Operators" binops n argTypeNAs with ClassRef cA -> cA

            // Now change Binop into a Call
            let call = CallStatic (cA, [l; r])
            eA.Item <- call
            
        | _ -> failwithf "Must call with a Binop, not a %s" <| eA.Item.ToString()


    /////////////
    let rec annotateTypesExpr (localVars:List<Ref>) (refs:Map<string,Ref>) (eA:ExprA) =
        eA.AddRefs(refs)
        let aTE (nextEA:ExprA) = match nextEA.PType with
            | Undef -> annotateTypesExpr localVars eA.Refs nextEA
            | _ -> ()
        eA.PType <- match eA.Item with
            | ConstInt (size, _) -> commonPtype !globals <| Int size
            | ConstBool _        -> commonPtype !globals Bool
            | Var n ->
                // See if it's a local ref
                match eA.GetRef(n) with
                    | Some ref -> ref.PType
                    | None -> failwithf "Can't find ref for %s" n;

            | VarStatic ptype -> !ptype

            | Binop (n, l, r) ->
                aTE l
                aTE r
                // Lower the binop to the appropriate Call and then annotate it
                lowerBinopToCall eA
                aTE eA
                eA.PType

            | Dot (dotEA, name) ->
                // Hopefully, the Call option should have taken care of procedures, leaving only variables to do

                aTE dotEA

                // First see if it is a static var
                let staticPos = match dotEA.Item with
                    | VarStatic ptype ->
                        let n = match !ptype with InitialType n -> n
                        match dotEA.GetRef n with
                            | None -> match getGlobal !globals eA.Namespace eA.Usings n with
                                | None -> failwithf "Cannot find class/interface %s" n
                                | Some nA -> match nA.GetRef <| VarKey name with
                                    | None -> failwithf "Cannot find existent member %s in %s" name nA.QName
                                    | Some (ClassRef cA) -> Some <| DotStatic (nA, cA)
                                    | _ -> failwithf "Can only get static class vars"
                            | _ -> None
                    | _ -> None

                // If it's not a static var it must be an instance var
                let newDot = 
                    if staticPos = None then
                        //aTE dotEA
                        match dotEA.PType with
                            | Type nA -> match nA.GetRef <| VarKey name with
                                | None -> failwithf "Cannot find member %s in %s" name nA.QName
                                | Some (ClassRef cA) -> DotInstance (dotEA, cA)
                                | _ -> failwithf "Something has gone terribly wrong 122324234"
                            | _ -> failwithf "Something has gone terribly wrong 86324"
                    else
                        staticPos.Value

                eA.Item <- newDot
                aTE eA
                eA.PType

            | DotStatic (nA, cA) -> cA.PType
            | DotInstance (eA, cA) -> cA.PType
            
            | Cast (ptype, castEA) ->
                let toNA = ptypeToNA !ptype
                aTE castEA
                let fromNA = ptypeToNA castEA.PType

                if not <| canCastTo fromNA toNA then
                    failwithf "Cannot cast %s to %s" fromNA.QName toNA.QName

                !ptype

            | Call (feA, exprAs) ->
                Seq.iter aTE exprAs

                let argTypeNAs = List.map (fun (eA:ExprA) -> ptypeToNA eA.PType) exprAs

                let staticCall (nA:NDA) dotName =
                    match getBestOverloadNA nA dotName argTypeNAs with
                        | ClassRef cA ->
                            if cA.IsStatic = NotStatic then
                                failwithf "Method %s in %s is not static" (NPKeyPretty <| classNPKey cA) nA.QName
                            CallStatic (cA, exprAs)

                let instanceCall (dotEA:ExprA) dotName =
                    match dotEA.PType with
                        | Type nA -> match getBestOverloadNA nA dotName argTypeNAs with
                            | ClassRef cA ->
                                if cA.IsStatic = Static then failwithf "Method %s in %s is static" (NPKeyPretty <| classNPKey cA) nA.QName
                                CallInstance (cA, dotEA, exprAs)
                            | InterfaceRef iA -> CallVirtual (iA, dotEA, exprAs)

                let localCall name =
                    let class_ = feA.NamespaceDecl.Value
                    match getBestOverloadNA class_ name argTypeNAs with
                        | ClassRef cA -> match cA.IsStatic with
                            | Static -> CallStatic (cA, exprAs)
                            | NotStatic ->
                                let this = ExprA(Var "this", Pos.NilPos)
                                CallInstance (cA, this, exprAs)
                        | _ -> failwithf "Can only call class methods"

                let expandMethod ptype typeParams isStatic methodName =
                    // Check to see if it exists first
                    let exNA = ptypeToNA ptype
                    let exNAT = exNA :> ITemplate
                    let templateNA = exNAT.Template.Value

                    let cirefToITemplate ciref = match ciref with | ClassRef cA -> cA :> ITemplate | InterfaceRef iA -> iA :> ITemplate 

                    let cirefmap = Map.ofSeq <| seq {
                        for k, unexpandedCI in Map.toSeq (templateNA :?> NamespaceDeclA).Refs do
                            let unexpandedCIT = cirefToITemplate unexpandedCI
                            if unexpandedCIT.TypeParams.Length = Seq.length typeParams then
                                let typeEnv = Seq.fold (fun state (k, v) -> Map.add k v state) exNAT.TypeEnv <| Seq.zip unexpandedCIT.TypeParams typeParams
                                match unexpandedCI with
                                    | ClassRef cA -> match cA.Item with
                                        | ClassProc (name, _, isSt, params_, _, _) when name = methodName && isSt = isStatic ->
                                            let found = ref !globals
                                            let key = nameParamsKey name (Templates.expandTemplateParams found typeEnv !params_) 0
                                            yield (key, (key, unexpandedCI, typeEnv))
                                        | _ -> ()
                                    | InterfaceRef iA -> match iA.Item with
                                        | InterfaceProc (name, params_, _) when name = methodName ->
                                            let found = ref !globals
                                            let key = nameParamsKey name (Templates.expandTemplateParams found typeEnv params_) 0
                                            yield (key, (key, unexpandedCI, typeEnv))
                    }

                    let ProcKey (_, argStrs, _), unexpandedCI, typeEnv = getBestOverload exNA.QName cirefmap methodName argTypeNAs

                    // See if we've already expanded out this one
                    match exNA.GetRef(ProcKey (Templates.paramedNameEnv methodName (cirefToITemplate unexpandedCI) typeEnv, argStrs, 0)) with
                        | Some (ClassRef cA) -> cA.Name
                        | Some (InterfaceRef iA) -> iA.Name
                        | None -> match unexpandedCI with
                            | ClassRef cA ->
                                // Expand CA and add it to the type's refs
                                let expandedCA = Templates.expandTemplateC globals typeEnv cA
                                expandedCA.NamespaceDecl <- Some exNA
                                exNA.AddRef(classNPKey expandedCA, ClassRef expandedCA)

                                // Annotate it's types
                                annotateTypesClass expandedCA

                                // Return new expanded name
                                expandedCA.Name
                            | InterfaceRef iA ->
                                let expandedIA = Templates.expandTemplateI globals typeEnv iA
                                expandedIA.NamespaceDecl <- Some exNA
                                exNA.AddRef(interfaceNPKey expandedIA, InterfaceRef expandedIA)

                                expandedIA.Name

                let loweredCall = match feA.Item with
                    // See if it's a static call
                    | Dot (dotEA, dotName) ->
                        aTE dotEA
                        match dotEA.Item with
                            | VarStatic ptype -> staticCall (ptypeToNA !ptype) dotName
                            | _ -> instanceCall dotEA dotName
                    | DotTemplate (dotEA, dotName, typeParams) ->
                        aTE dotEA
                        match dotEA.Item with
                            | VarStatic ptype ->
                                expandMethod !ptype !typeParams Static dotName
                                |> staticCall (ptypeToNA !ptype)
                            | _ -> 
                                expandMethod dotEA.PType !typeParams NotStatic dotName
                                |> instanceCall dotEA

                    | Var n -> localCall n
                
                eA.Item <- loweredCall
                aTE eA
                eA.PType

            | CallStatic (cA, exprAs) -> cA.PType
            | CallInstance (cA, feA, exprAs) -> 
                aTE feA
                cA.PType
            | CallVirtual (iA, feA, exprAs) -> 
                aTE feA
                iA.PType         
               
            | Assign (lvalue, innerExprA) ->
                aTE lvalue
                aTE innerExprA
                // Check they have the same types
                if not (lvalue.PType = Undef) && not (lvalue.PType = innerExprA.PType) then
                    failwithf "Assignment expected type %s but got %s" (lvalue.PType.ToString()) (innerExprA.PType.ToString())  
                innerExprA.PType
            | DeclVar (name, assignA) ->
                // Since we have declared a variable, add a reference object for it
                let ref = Ref(name, Undef, LocalRef)
                assignA.AddRef(name, ref)
                aTE assignA
                ref.PType <- assignA.PType
                eA.AddRef(name, ref)
                localVars.Add(ref)
                assignA.PType
            | Return exprA ->
                aTE exprA
                commonPtype !globals Unit
            | ReturnVoid ->
                commonPtype !globals Unit
            | If (test, then_, else_) ->
                aTE test
                aTE then_
                aTE else_
                then_.PType
            | While (test, body) ->
                aTE test
                aTE body
                commonPtype !globals Unit
            | Seq (e1A, e2A) ->
                aTE !e1A
                // Since e2A is lexically below and and in the same scope as e1A, all 
                // e1A's references also appear in e2A
                annotateTypesExpr localVars (!e1A).Refs !e2A
                commonPtype !globals Unit
            | Nop ->
                commonPtype !globals Unit


    /////////////
    and annotateTypesClass (cA:ClassDeclA) =

        let eA = cA.ExprA
        let initRefs = cA.GetRefs ()

        let localVars = new List<Ref>()

        // Only annotate the expression if not a ctor
        if not cA.IsCtor then
            annotateTypesExpr localVars initRefs eA

        let localVarList = List.ofSeq localVars

        eA.AddLocalVars localVarList
        cA.AddLocalVars localVarList


    /////////////
    let annotateTypesNamespace (nA:NamespaceDeclA) =
        match nA.Item with
            | Class (name, vis, isStruct, ifaces, cAs) ->
                Seq.iter annotateTypesClass  cAs
            | _ -> ()



    Seq.iter annotateTypesNamespace <| globalsToNAs !globals