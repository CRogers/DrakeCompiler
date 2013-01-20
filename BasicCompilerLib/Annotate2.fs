module Annotate2

open Tree

(*

Now we have all the appropriate references, go through and annotate the types of all the expression and in the
process check them

*)

let annotateTypesInterface (iA:InterfaceDeclA) = ()

let annotateTypesNamespace (nA:NamespaceDeclA) = ()

let annotateTypesTop (tA:TopDeclA) = ()

let annotateTypesCU (cu:CompilationUnit) = ()


let annotateTypesExpr (eA:ExprA) = ()
    


let annotateTypesProgram (program:Program) =
    let exprFunc (annot:Annot) =
        match annot with
            | :? ExprA as eA ->
                if eA.PType = Undef then
                    annotateTypesExpr eA
                ()
            | _ -> () 

    iterAST foldASTProgram exprFunc program