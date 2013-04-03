module AnnotateTypes

open Tree
open Builtins
open Print
open Util
open System.Collections.Generic

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let annotateTypes (globals:GlobalStore) (binops:BinopStore) (program:seq<NamespaceDeclA>) =

    /////////////
    let lowerBinopToCall (eA:ExprA) = match eA.Item with
        | Binop (n, l, r) ->
            let key = namePTypesKey n <| List.map (fun (eA:ExprA) -> eA.PType) [l; r]
            let value = match Map.tryFind key binops with
                | Some v -> v
                | None -> failwithf "Cannot find an appropriate static method to call for binop %s" <| fmt eA.Item

            // Check to see ww have only one possible thing to call
            if Seq.length value > 1 then
                System.String.Join(", ", Seq.map (fun (cA:ClassDeclA) -> cA.QName) value)
                |> failwithf "Cannot choose between methods %s"

            let cA = Seq.head value

            // Now change Binop into a Call
            let call = CallStatic (cA, [l; r])
            eA.Item <- call
        | _ -> failwithf "Must call with a Binop, not a %s" <| fmt eA.Item


    /////////////
    let rec annotateTypesExpr (localVars:List<Ref>) (refs:Map<string,Ref>) (eA:ExprA) =
        eA.AddRefs(refs)
        let aTE (nextEA:ExprA) = match nextEA.PType with
            | Undef -> annotateTypesExpr localVars eA.Refs nextEA
            | _ -> ()
        eA.PType <- match eA.Item with
            | ConstInt (size, _) -> commonPtype globals <| Int size
            | ConstBool _        -> commonPtype globals Bool
            | ConstUnit          -> commonPtype globals Unit
            | Var n ->
                // See if it's a local ref
                match eA.GetRef(n) with
                    | Some ref -> ref.PType
                    | None -> failwithf "Can't find ref for %s" n

            | Binop (n, l, r) ->
                aTE l
                aTE r
                // Lower the binop to the appropriate Call and then annotate it
                lowerBinopToCall eA
                aTE eA
                eA.PType

            | Dot (dotEA, name) ->
                // Hopefully, the Call option should have taken care of procedures, leaving only variables to do

                // First see if it is a static var
                let staticPos = match dotEA.Item with
                    | Var n -> match dotEA.GetRef n with
                        | None -> match getGlobal globals eA.Namespace eA.Usings n with
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
                        aTE dotEA
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

            | Call (feA, exprAs) ->
                Seq.iter aTE exprAs

                let keygen n = namePTypesKey n <| List.map (fun (eA:ExprA) -> eA.PType) exprAs                    

                let staticCall typeName dotName = 
                    let key = keygen dotName
                    match getGlobal globals eA.Namespace eA.Usings typeName with
                        | None -> failwithf "Cannot find the static type %s" typeName
                        | Some nA -> match nA.GetRef(key) with
                            | Some (ClassRef cA) ->
                                if cA.IsStatic = NotStatic then
                                    failwithf "Method %s in %s is not static" (NPKeyPretty key) nA.QName
                                CallStatic (cA, exprAs)
                            | None -> failwithf "Cannot find static method %s in %s" (NPKeyPretty key) nA.QName
                            | _ -> failwithf "Can only call static members on classes"

                let instanceCall dotEA dotName =
                    aTE dotEA
                    let key = keygen dotName
                    match dotEA.PType with
                        | Type nA -> match nA.GetRef(key) with
                            | Some (ClassRef cA) ->
                                if cA.IsStatic = Static then failwithf "Method %s in %s is static" (NPKeyPretty key) nA.QName
                                CallInstance (cA, dotEA, exprAs)
                            | Some (InterfaceRef iA) -> CallVirtual (iA, dotEA, exprAs)
                            | None -> failwithf "No such method %s in %s" (NPKeyPretty key) nA.QName

                let localCall name =
                    let class_ = feA.NamespaceDecl.Value
                    let key = keygen name
                    match class_.GetRef key with
                        | None -> failwithf "Can't find any method %s in %s" name class_.QName
                        | Some (ClassRef cA) -> match cA.IsStatic with
                            | Static -> CallStatic (cA, exprAs)
                            | NotStatic ->
                                let this = ExprA(Var "this", Pos.NilPos)
                                CallInstance (cA, this, exprAs)
                        | _ -> failwithf "Can only call class methods"


                let loweredCall = match feA.Item with
                    // See if it's a static call
                    | Dot (dotEA, dotName) -> match dotEA.Item with
                        | Var n -> match eA.GetRef n with
                            | None -> staticCall n dotName
                            | Some _ -> instanceCall dotEA dotName
                        | _ -> instanceCall dotEA dotName
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
            | Print exprA ->
                aTE exprA
                exprA.PType
            | Return exprA ->
                aTE exprA
                commonPtype globals Unit
            | ReturnVoid ->
                commonPtype globals Unit
            | If (test, then_, else_) ->
                aTE test
                aTE then_
                aTE else_
                then_.PType
            | While (test, body) ->
                aTE test
                aTE body
                commonPtype globals Unit
            | Seq (e1A, e2A) ->
                aTE !e1A
                // Since e2A is lexically below and and in the same scope as e1A, all 
                // e1A's references also appear in e2A
                annotateTypesExpr localVars (!e1A).Refs !e2A
                commonPtype globals Unit
            | Nop ->
                commonPtype globals Unit


    /////////////
    let annotateTypesClass instanceLevelRefs staticLevelRefs (cA:ClassDeclA) =
        let eA = match cA.Item with
            | ClassVar (name, vis, isStatic, ptype, eA) -> eA
            | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
                // Add params as refs
                Seq.iter (fun (p:Param) -> eA.AddRef(p.Name, Ref(p.Name, p.PType, LocalRef))) !params_
                // If an instance method add the 'this' param
                if isStatic = NotStatic then
                    eA.AddRef("this", Ref("this", Type <| cA.NamespaceDecl.Value, LocalRef))
                eA

        // Add ctor ref if it is a static class
        let initRefs = match cA.IsStatic with
            | NotStatic -> instanceLevelRefs
            | Static -> staticLevelRefs

        let localVars = new List<Ref>()

        // Only annotate the expression if not a ctor
        if not cA.IsCtor then
            annotateTypesExpr localVars initRefs eA

        eA.AddLocalVars(List.ofSeq localVars)
        cA.AddLocalVars(List.ofSeq localVars)


    /////////////
    let annotateTypesNamespace (nA:NamespaceDeclA) =
        match nA.Item with
            | Class (name, vis, isStruct, ifaces, cAs) ->
                // Ref for the ctor
                let ctorRef = Ref(name, Type nA, StaticProcRef)
                nA.CtorCA.Ref <- ctorRef

                // Make refs for the cAs so they can reference eachother. Left for static, Right for not static
                let classLevelRefs = 
                    List.map (fun (cA:ClassDeclA) ->
                        let ref = Ref(cA.Name, cA.PType, if cA.IsProc then (if cA.IsStatic = Static then StaticProcRef else InstanceProcRef) else InstanceVarRef)
                        cA.Ref <- ref
                        either (isStatic cA.IsStatic) (cA.Name, ref)) cAs

                let instanceLevelRefs = allEithers classLevelRefs |> Map.ofSeq
                let staticLevelRefs = Seq.append [name, ctorRef] (lefts classLevelRefs) |> Map.ofSeq

                Seq.iter (annotateTypesClass instanceLevelRefs staticLevelRefs) cAs
            | _ -> ()



    Seq.iter annotateTypesNamespace program