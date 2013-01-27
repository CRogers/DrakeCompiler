module Annotate2

open Tree
open Builtins
open Print

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let rec annotateTypesExpr (globals:Map<string,NamespaceDeclA>) (localVars:list<Ref>) (eA:ExprA) =
    eA.AddLocalVars(localVars)
    let aTE nextEA = annotateTypesExpr globals eA.LocalVars nextEA
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
                        | Some (ClassRef (_,cA))     -> cA.PType
                        | Some (InterfaceRef (_,iA)) -> iA.PType
                // Static
                | StaticType typeName ->
                    match Map.tryFind typeName globals with
                        | None -> failwithf "Cannot find non-existant class/interface %s" name
                        | Some nA ->
                            match nA.GetRef(name) with
                                | None -> failwithf "Can't access non existant static member %s" name
                                | Some (ClassRef (_,cA)) -> match cA.IsStatic with
                                    | Static -> cA.PType
                                    | NotStatic -> failwithf "Can only call *static* classrefs"
                                | _ -> failwithf "Can only call static class refs"

        | Call (feA, exprAs) ->
            aTE feA
            Seq.iter aTE exprAs
            let callingArgPtypes = List.map (fun (eA:ExprA) -> eA.PType) exprAs

            match feA.PType with
                | PFunc (argTypes, returnType) -> 
                    if not (callingArgPtypes = argTypes) then
                        failwithf "Must call function with the right arguments!"
                    returnType
                | _ ->
                    failwithf "Can only call function types!"
            
        | Assign (name, innerExprA) ->
            aTE innerExprA
            innerExprA.PType
        | DeclVar (name, assignA) ->
            aTE assignA
            // Since we have declared a variable, add a reference object for it
            let ref = Ref(name, assignA.PType)
            eA.AddRef(name, ref)
            eA.AddLocalVar(ref)
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
            annotateTypesExpr globals e1A.LocalVars e2A
            commonPtype Unit

let annotateTypesClass globals (cA:ClassDeclA) =
    match cA.Item with
        | ClassVar (name, vis, isStatic, ptype, eA) ->
            annotateTypesExpr globals [] eA
        | ClassProc (name, vis, isStatic, isCtor, params_, returnType, eA) ->
            annotateTypesExpr globals [] eA

let annotateTypesNamespace globals (nA:NamespaceDeclA) =
    match nA.Item with
        | Class (name, vis, cAs) ->
            Seq.iter (annotateTypesClass globals) cAs
        | _ -> ()

let annotateTypes (globals:Map<string,NamespaceDeclA>) (program:seq<NamespaceDeclA>) =
    Seq.iter (annotateTypesNamespace globals) program