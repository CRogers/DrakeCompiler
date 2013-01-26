module Annotate2

open Tree
open Builtins
open Print

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let rec annotateTypesExpr (localVars:list<RefA>) (eA:ExprA) =
    eA.AddLocalVars(localVars)
    let aTE nextEA = annotateTypesExpr eA.LocalVars nextEA
    eA.PType <- match eA.Item with
        | ConstInt (size, _) -> commonPtype <| Int size
        | ConstBool _        -> commonPtype Bool
        | ConstUnit          -> commonPtype Unit
        | Var n              -> eA.GetRef(n).PType
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
                // Namespace::ClassInteraface.something
                | RefType (NamespaceLevelRef (qn, cIA)) -> match cIA.Item with
                    // Namespace::Class
                    | Class _ -> match cIA.GetRef(name).Ref with
                        | ClassLevelRef (qn, cA) ->
                            if cA.IsStatic then
                                cA.PType
                            else
                                failwithf "Can only access static vars"
                        | _ -> failwithf "Something went wrong HEGHEXB"
                    // Namespace::Interface
                    | Interface _ -> match cIA.GetRef(name).Ref with
                        | InterfaceLevelRef (qn, iA) ->
                            iA.PType
                // 
                | UserType name ->
                    failwithf "Not implemented"
                        

        | Call (feA, exprAs) ->
            aTE feA
            Seq.iter aTE exprAs
            let callingArgPtypes = List.map (fun (eA:ExprA) -> eA.PType) exprAs

            match feA.PType with
                | RefType rA -> match rA.FunctionPType with
                    | Some (PFunc (argTypes, returnType)) -> 
                        if not (callingArgPtypes = argTypes) then
                            failwithf "Must call function with the right arguments!"
                        returnType
                    | None ->
                        failwithf "Can only call function types!"
            
        | Assign (name, innerExprA) ->
            aTE innerExprA
            innerExprA.PType
        | DeclVar (name, assignA) ->
            aTE assignA
            // Since we have declared a variable, add a reference object for it
            let ref = RefA(name, VarRef (name, assignA.PType))
            eA.AddRef(ref)
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
            annotateTypesExpr e1A.LocalVars e2A
            commonPtype Unit


let annotateTypes (program:Program) =
    let exprFunc (annot:Annot) =
        match annot with
            | :? ExprA as eA ->
                if eA.PType = Undef then
                    annotateTypesExpr [] eA
                ()
            | _ -> () 

    iterAST foldASTProgram exprFunc program