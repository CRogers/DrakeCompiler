module Tree

open Print
open LLVM.Generated.Core
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open System.Collections.ObjectModel
open System

type Op = 
    | Add | Sub | Mul | Div
    | BoolAnd | BoolOr | Not
    | Lt | Gt | LtEq | GtEq | Eq

type PType = 
    | Undef
    | UserType of string
    | PFunc of (*arg types*) list<PType> * (*return type*) PType
    with
    override x.ToString() = fmt x

type CommonPtype =
    | Unit
    | Int of int
    | Bool
    | String

let cpPrefix x = "System::" + x 

let commonPtype x = UserType (cpPrefix <| match x with
    | Unit   -> "Unit"
    | Int i  -> "Int" + i.ToString()
    | Bool   -> "Bool"
    | String -> "String")

type Pos(startPos:Position, endPos:Position) =
    member x.StartPos = startPos
    member x.EndPos = endPos
    override x.ToString() = sprintf "s(%i,%i)e(%i,%i)" x.StartPos.Line x.StartPos.Column x.EndPos.Line x.EndPos.Column

type RefType = Local | Parameter

type Ref(name:string, ptype:PType, reftype: RefType) =
    member x.Name = name
    member x.PType = ptype
    member x.RefType = reftype
    member val ValueRef = new ValueRef(nativeint 0xDEAD0000) with get, set
    member x.IsUninitialised = x.ValueRef.Ptr.ToInt32() = 0xDEAD0000
    override x.ToString() = sprintf "%s:%s%s" x.Name (fmt x.PType) (if x.IsUninitialised then ":Uninit" else "")

type Param(name: string, ptype: PType) =
    member x.Name = name
    member x.PType = ptype
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)

type Visibility =
    | Private
    | Public

type IVisibility =
    abstract Visibility : Visibility

type GlobalDeclStore() =
    let dict = new Dictionary<string, Dictionary<string, NamespaceDeclA>>()

    member x.AddNamespaceDecl(ndeclA:NamespaceDeclA) =
        let namespace_ = ndeclA.Namespace
        if not <| dict.ContainsKey(namespace_) then
            dict.[namespace_] <- new Dictionary<string, NamespaceDeclA>()
        dict.[namespace_].Add(ndeclA.Name, ndeclA)

    member x.GetNamespaceDecl(currentNamespace, usings:list<string>, possiblyQualifiedName:string) = 
        let split = possiblyQualifiedName.Split([|"::"|], StringSplitOptions.None)
        let declName = Seq.last <| split

        // Case 1: The Namespace::Is::Fuly::Qualified
        if split.Length = 0 then
            let namespaceSeq = Seq.take (split.Length-1) split
            let namespace_ = String.Join("::", namespaceSeq)
            dict.[namespace_].[declName]
        // Case 2: Not qualified
        else
            let tryGet namespace_ =
                if dict.[currentNamespace].ContainsKey(declName) then
                    Some <| dict.[currentNamespace].[declName]
                else
                    None
            // TODO: Error on multiple possibilities found
            // Check current namespace first
            match tryGet currentNamespace with
                | Some decl -> decl
                | None ->
                    // Check using namespaces in order
                    let possibles = Seq.map tryGet usings
                    let numPossibles = Seq.sumBy (fun x -> if x = None then 0 else 1) possibles
                    if numPossibles = 0 then
                        failwithf "Cannot find any decl of name %s" possiblyQualifiedName
                    else if numPossibles > 1 then
                        failwithf "Multiple possible decls found for %s" possiblyQualifiedName
                    else
                        Option.get <| Seq.find Option.isSome possibles
                        
        


        

and [<AbstractClass>] Annot(pos:Pos) =
    let mutable refs:Map<string,Ref> = Map.empty;
    let filterRefs refType =
        Map.filter (fun k (v:Ref) -> v.RefType = refType) refs
        |> Map.toSeq |> Seq.map (fun (k,v) -> v)

    member x.Refs = refs
    member x.AddRef(ref:Ref) = refs <- refs.Add(ref.Name, ref)
    member x.AddRefs(refs) = Map.iter (fun name ref -> x.AddRef(ref)) refs
    member x.GetRef(name) = Map.find name refs
    member x.LocalRefs with get () = filterRefs Local
    member x.ParamRefs with get () = filterRefs Parameter

    member val GlobalDecls:GlobalDeclStore = GlobalDeclStore() with get, set

    member val LocalVars:list<Ref> = [] with get, set

    member x.Pos = pos
    member val PType = Undef with get, set
    abstract member ItemObj : obj
    member x.ItemAs<'a>():'a = downcast x.ItemObj

    member val UsingContext:list<string> = [] with get, set
    member val Namespace = "" with get, set

    override x.ToString() = fmt x.ItemObj + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType
        (*":" + x.Pos.ToString() +
        ":" + fmt (List.ofSeq <| Seq.map (fun (kvp:KeyValuePair<string,Ref>) -> kvp.Value) x.Refs)*)
        

and Expr =
    | ConstInt of (*size*) int * (*value*) int64
    | ConstBool of bool
    | ConstUnit
    | Var of string
    | Binop of Op * ExprA * ExprA
    | Call of string * list<ExprA>
    | Assign of string * ExprA
    | DeclVar of string * (*Assign*) ExprA
    | Print of ExprA
    | Return of ExprA
    | If of ExprA * ExprA * ExprA
    | While of ExprA * ExprA
    | Seq of ExprA * ExprA

and ExprA(item:Expr, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = item :> obj


and ClassDecl =
    | ClassVar of string * Visibility * PType * ExprA
    | ClassProc of (*name*) string * Visibility * (*params*) list<Param> * (*returnType*) PType * (*body*) ExprA
    

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member x.Visibility = match x.Item with
        | ClassVar (_, vis, _, _) -> vis
        | ClassProc (_, vis, _, _, _) -> vis

    member x.Name = match x.Item with
        | ClassVar (name, _, _, _) -> name
        | ClassProc (name, _, _, _, _) -> name


and InterfaceDecl =
    | InterfaceProc of (*name*) string * (*params*) list<Param> * (*returnType*) PType
    

and InterfaceDeclA(item:InterfaceDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member x.Name = match x.Item with
        | InterfaceProc (name, _, _) -> name

and NamespaceDecl =
    | Class of string * Visibility * list<ClassDeclA>
    | Interface of string * Visibility * list<InterfaceDeclA>

and NamespaceDeclA(item:NamespaceDecl, pos:Pos) = 
    inherit Annot(pos)
    member x.Item:NamespaceDecl = item
    override x.ItemObj = upcast item

    member x.Visibility = match x.Item with
            | Class (_, vis, _) -> vis
            | Interface (_, vis, _) -> vis

    member x.Name = match x.Item with
        | Class (name, _, _) -> name
        | Interface (name, _, _) -> name

    member x.QualifiedName = x.Namespace + "::" + x.Name

    member x.GetClassDecl(name) = match x.Item with
        | Class (_, _, cdeclAs) -> List.tryFind (fun (c:ClassDeclA) -> c.Name = name) cdeclAs

    member x.GetClassDecl(name, vis) = Option.exists (fun (cd:ClassDeclA) -> cd.Visibility = vis) <| x.GetClassDecl(name)

    member x.GetInterfaceDecl(name) = match x.Item with
        | Interface (_, _, ideclAs) -> List.tryFind (fun (i:InterfaceDeclA) -> i.Name = name) ideclAs


type TopDecl =
    | Using of string
    | Namespace of string * list<NamespaceDeclA>

type TopDeclA(item:TopDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item


type CompilationUnit = list<TopDeclA>
type Program = list<CompilationUnit>

type Func(name: string, func: ValueRef, params: Map<string, ValueRef>) =
    member x.Name = name
    member x.Func = func
    member x.Params = params

type Environ(module_: ModuleRef, enclosingFunc: Ref) =
    member x.Module = module_
    member x.EnclosingFunc = enclosingFunc


let rec foldASTExpr (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (exprA:ExprA) =
    let fAST e = foldASTExpr branchFunc leafFunc e
    let bf1 branch e = branchFunc branch <| [fAST e]
    let bf branch es = branchFunc branch <| List.map fAST es
    match exprA.Item with
        | ConstInt _ -> leafFunc exprA
        | ConstBool _ -> leafFunc exprA
        | ConstUnit -> leafFunc exprA
        | Var n -> leafFunc exprA
        | Binop (op, l, r) -> bf exprA [l; r]
        | Call (name, exprAs) -> bf exprA exprAs
        | Assign (name, innerExprA) -> bf1 exprA innerExprA
        | DeclVar (name, assignA) -> bf1 exprA assignA
        | Print e -> bf1 exprA e
        | Return e -> bf1 exprA e
        | If (test, then_, else_) -> bf exprA [test; then_; else_]
        | While (test, body) -> bf exprA [test; body]
        | Seq (e1A, e2A) -> bf exprA [e1A; e2A]

let foldASTClassDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (cdecl:ClassDeclA) =
    let fAST e = foldASTExpr branchFunc leafFunc e
    match cdecl.Item with
        | ClassVar (_, _, _, exprA) -> branchFunc cdecl [fAST exprA]
        | ClassProc (_, _, _, _, exprA) -> branchFunc cdecl [fAST exprA]

let foldASTInterfaceDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (idecl:InterfaceDeclA) =
    leafFunc idecl

let foldASTNamespaceDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (ndecl:NamespaceDeclA) =
    let fAST fASTFunc declAs = branchFunc ndecl <| List.map (fASTFunc branchFunc leafFunc) declAs
    match ndecl.Item with
        | Class (_, _, cdeclAs) -> fAST foldASTClassDecl cdeclAs
        | Interface (_, _, ideclAs) -> fAST foldASTInterfaceDecl ideclAs

let foldASTTopDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (tdecl:TopDeclA) =
    match tdecl.Item with
        | Namespace (_, namespaces) -> branchFunc tdecl <| List.map (foldASTNamespaceDecl branchFunc leafFunc) namespaces
        | Using _ -> leafFunc tdecl

let foldASTCompilationUnit branchFunc leafFunc (cu:CompilationUnit) =
    List.map (foldASTTopDecl branchFunc leafFunc) cu

let foldASTProgram branchFunc leafFunc (prog:Program) =
    List.map (foldASTCompilationUnit branchFunc leafFunc) prog

let iterAST foldFunc (f:Annot -> 'a) =
    let bf x (_:list<'a>) = f x
    foldFunc bf f
