module Mixins

open Tree
open ASTBuildUtils

let fixMixinReferences (program:seq<NDA>) =
    for eA in getAllExprs program do
        iterAST foldASTExpr (fun (annot:Annot) ->
            let eA = annot :?> ExprA
            match eA.Item with
                | VarStatic ptype -> 
                    let replaceMaybe (nA:NDA) f = match nA.MixinClass with
                            | Some mixinClass -> ptype := f mixinClass
                            | None -> ()

                    match !ptype with
                        | Type nA -> replaceMaybe nA Type
                        | ParamedType (Type nA, tps) -> replaceMaybe nA (fun nA -> ParamedType (Type nA, tps))
                        | _ -> ()
                | _ -> ()) eA


let makeForwardingMethods (program:seq<NDA>) =   

    let makeForwardingMethodsC (cA:CDA) =
        match cA.MixinForwarding with
            | Some mf -> 
                let mixinNA = ptypeToNA cA.PType
                let mixinName = cA.Name

                // Get all the methods we could possibly forward
                let allInstanceDescs =
                    seq { for iA in getInterfaceDecls [mixinNA] -> (iA.Name, iA.Params, iA.PType) }

                // Checks to see if a method description is in a set of integers
                let matchesASig sigList (name, params_, retType) =
                    let argTypes = [for (p:Param) in params_ -> p.PType]
                    Seq.exists (fun (sigName, sigArgTypes, sigRetType) ->
                        sigName = name && sigArgTypes = argTypes && sigRetType = retType) sigList

                // Find the appropriate methods to forward
                let forwardingMethodDesc = match mf with
                    | ForwardAll -> allInstanceDescs
                    | ForwardMethods sigList -> Seq.filter (matchesASig sigList) allInstanceDescs
                    | HidingMethods sigList -> Seq.filter (not << matchesASig sigList) allInstanceDescs
                    | HidingAll -> Seq.empty

                // Make forwarding stubs for them
                let fstubs = seq { for (methodName, params_, retType) in forwardingMethodDesc ->
                    // Call the real method: mixinName.methodName(x1, x2, ..., xN)
                    let var = eanp <| Var mixinName
                    let dot = eanp <| Dot (var, methodName)
                    let paramVars = [for p in params_ -> eanp <| Var p.Name]
                    let call = eanp <| Call (dot, paramVars)
                    
                    // See if it's a void method or one that returns a value
                    let ret =
                        if (ptypeToNA retType).QName = commonPtypeStr Unit then
                            Seq (ref call, ref <| eanp ReturnVoid)
                        else Return call
                        |> eanp

                    canp <| ClassProc (methodName, Public, NotStatic, ref params_, ref retType, ret)
                }

                // Add the stubs to the class
                let mixinDeclaringClass = cA.NamespaceDecl.Value
                for fstub in fstubs do
                    mixinDeclaringClass.AddRef(classNPKey fstub, ClassRef fstub)
                    // Annotate some stuff on them
                    iterAST foldASTClassDecl (fun (annot:Annot) -> annot.NamespaceDecl <- cA.NamespaceDecl) fstub
                   
            | _ -> ()
                    

    for cA in getClassDecls program do
        makeForwardingMethodsC cA
        