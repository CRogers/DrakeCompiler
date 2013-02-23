module Annotate2

open Tree
open Builtins
open Print
open Util
open System.Collections.Generic

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let annotateTypesLvalue (refs:Map<string,Ref>) (eA:ExprA) =
    eA.AddRefs(refs)
    eA.PType <- match eA.Item with
        | Var n -> match eA.GetRef(n) with
            | Some r -> r.PType
            | None -> failwithf "lvalue annotate fail"


let rec annotateTypesExpr (globals:GlobalStore) (localVars:List<Ref>) (refs:Map<string,Ref>) (eA:ExprA) =
    eA.AddRefs(refs)
    let aTE nextEA = annotateTypesExpr globals localVars eA.Refs nextEA
    eA.PType <- match eA.Item with
        | ConstInt (size, _) -> commonPtype <| Int size
        | ConstBool _        -> commonPtype Bool
        | ConstUnit          -> commonPtype Unit
        | Var n ->
            // See if it's a local ref
            match eA.GetRef(n) with
                | Some ref -> ref.PType
                | None ->
                    // Look in globals for it
                    match getGlobal globals eA.Namespace eA.Usings n with
                        | Some nA -> StaticType nA.QName
                        | None -> failwithf "Can't find static user type %s" n
        | Binop (op, l, r) ->
            aTE l
            aTE r
            match binopToType op l.PType r.PType with
            | None -> failwithf "Incorrect type for binary operation:\n%s" (fmt eA.Item)
            | Some ptype -> ptype

        | Dot (dotEA, name) ->
            // We can dot:
            // Classes for static methods/vars
            // Classes/Interfaces instances for procs
            aTE dotEA
            match dotEA.PType with
                // Instance
                | UserType typeName ->
                    let nA = Map.find typeName globals
                    match nA.GetRef(name) with
                        | None -> failwithf "Cannot access non existent member %s" name
                        | Some (ClassRef cA)     -> cA.PType
                        | Some (InterfaceRef iA) -> iA.PType
                // Static
                | StaticType typeName ->
                    match Map.tryFind typeName globals with
                        | None -> failwithf "Cannot find non-existant class/interface %s" name
                        | Some nA ->
                            match nA.GetRef(name) with
                                | None -> failwithf "Can't access non existant static member %s" name
                                | Some (ClassRef cA) -> match cA.IsStatic with
                                    | Static -> cA.PType
                                    | NotStatic -> failwithf "Can only call *static* classrefs"
                                | _ -> failwithf "Can only call static class refs"

        | Call (feA, exprAs) ->
            annotateTypesLvalue eA.Refs feA
            Seq.iter aTE exprAs
            let callingArgPtypes = List.map (fun (eA:ExprA) -> eA.PType) exprAs

            match feA.PType with
                | PFunc (argTypes, returnType) -> 
                    if not (callingArgPtypes = argTypes) then
                        failwithf "Must call function with the right arguments!"
                    returnType
                | _ ->
                    failwithf "Can only call function types!"
            
        | Assign (lvalue, innerExprA) ->
            annotateTypesLvalue eA.Refs lvalue
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
            commonPtype Unit
        | If (test, then_, else_) ->
            aTE test
            aTE then_
            aTE else_
            then_.PType
        | While (test, body) ->
            aTE test
            aTE body
            commonPtype Unit
        | Seq (e1A, e2A) ->
            aTE e1A
            // Since e2A is lexically below and and in the same scope as e1A, all 
            // e1A's references also appear in e2A
            annotateTypesExpr globals localVars e1A.Refs e2A
            commonPtype Unit

let annotateTypesClass globals instanceLevelRefs staticLevelRefs (cA:ClassDeclA) =
    let eA = match cA.Item with
        | ClassVar (name, vis, isStatic, ptype, eA) -> eA
        | ClassProc (name, vis, isStatic, params_, returnType, eA) ->
            // Add params as refs
            Seq.iter (fun (p:Param) -> eA.AddRef(p.Name, Ref(p.Name, p.PType, LocalRef))) !params_
            // If an instance method add the 'this' param
            if isStatic = NotStatic then
                eA.AddRef("this", Ref("this", UserType <| cA.NamespaceDecl.Value.QName, LocalRef))
            eA

    // Add ctor ref if it is a static class
    let initRefs = match cA.IsStatic with
        | NotStatic -> instanceLevelRefs
        | Static -> staticLevelRefs

    let localVars = new List<Ref>()
    annotateTypesExpr globals localVars initRefs eA

    eA.AddLocalVars(List.ofSeq localVars)
    cA.AddLocalVars(List.ofSeq localVars)

let annotateTypesNamespace globals (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, isStruct, cAs) ->
            // Ref for the ctor
            let ctorRef = Ref(name, PFunc ([], UserType nA.QName), StaticProcRef)
            nA.CtorRef <- ctorRef

            // Make refs for the cAs so they can reference eachother. Left for static, Right for not static
            let classLevelRefs = 
                List.map (fun (cA:ClassDeclA) ->
                    let ref = Ref(cA.Name, cA.PType, if cA.IsProc then (if cA.IsStatic = Static then StaticProcRef else InstanceProcRef) else InstanceVarRef)
                    cA.Ref <- ref
                    either (isStatic cA.IsStatic) (cA.Name, ref)) cAs

            let instanceLevelRefs = allEithers classLevelRefs |> Map.ofSeq
            let staticLevelRefs = Seq.append [name, ctorRef] (lefts classLevelRefs) |> Map.ofSeq

            Seq.iter (annotateTypesClass globals instanceLevelRefs staticLevelRefs) cAs
        | _ -> ()

let annotateTypes (globals:GlobalStore) (program:seq<NamespaceDeclA>) =
    Seq.iter (annotateTypesNamespace globals) program