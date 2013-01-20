module Tree

open Print
open LLVM.Generated.Core
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open System.Collections.ObjectModel
open System

let qualifiedName namespace_ classInterfaceName (extraNames:seq<string>) =
    namespace_ + "::" + classInterfaceName + "." + String.Join(".", extraNames)

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

type Param(name: string, ptype: PType) =
    member x.Name = name
    member x.PType = ptype
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)

let paramsReturnTypeToPtype (params_:list<Param>) returnType =
    let ptypeParams = List.map (fun (p:Param) -> p.PType) params_
    PFunc (ptypeParams, returnType)

type Visibility =
    | Private
    | Public

type IVisibility =
    abstract Visibility : Visibility                    
        

type Ref =
    | VarRef of string * PType
    | ClassVarRef of string * ClassDeclA
    | ClassProcRef of string * ClassDeclA
    | InterfaceProcRef of string * InterfaceDeclA
    | ClassRef of string * NamespaceDeclA
    | InterfaceRef of string * NamespaceDeclA

and RefA(name: string, ref: Ref) =
    member x.Name = name
    member x.Ref = ref
    member val ValueRef = new ValueRef(nativeint 0xDED) with get, set
    member x.IsUninitialised = x.ValueRef.Ptr.ToInt32() = 0xDED

    member x.QualifiedName = match ref with
        | VarRef (qn, _)           -> qn
        | ClassVarRef (qn, _)      -> qn
        | ClassProcRef (qn, _)     -> qn
        | InterfaceProcRef (qn, _) -> qn
        | ClassRef (qn, _)         -> qn
        | InterfaceRef (qn, _)     -> qn

    override x.ToString() = 
        match ref with
            | VarRef (_, ptype)   -> fmt ptype
            | ClassVarRef _       -> "ClassVar"
            | ClassProcRef _      -> "ClassProc"
            | InterfaceProcRef _  -> "InterfaceProc"
            | ClassRef _          -> "Class"
            | InterfaceRef _      -> "Namespace" 
        |> sprintf "%s:%s" x.QualifiedName


and [<AbstractClass>] Annot(pos:Pos) =
    let mutable refs:Map<string,RefA> = Map.empty;

    member x.Refs = refs
    member x.AddRef(ref:RefA) = refs <- refs.Add(ref.Name, ref)
    member x.AddRefs(refs) = Map.iter (fun name ref -> x.AddRef(ref)) refs
    member x.AddRefs(refs) = Seq.iter (fun ref -> x.AddRef(ref)) refs
    member x.GetRef(name) = Map.find name refs

    member val LocalVars:list<Ref> = [] with get, set

    member x.Pos = pos
    abstract member ItemObj : obj
    member x.ItemAs<'a>():'a = downcast x.ItemObj

    override x.ToString() = fmt x.ItemObj
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

    member val PType = Undef with get, set

    override x.ToString() = base.ToString() + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType


and ClassDecl =
    | ClassVar of string * Visibility * PType * ExprA
    | ClassProc of (*name*) string * Visibility * (*static*) bool * (*ctor*) bool * (*params*) list<Param> * (*returnType*) PType * (*body*) ExprA
    

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member val PType = Undef with get, set

    member x.Visibility = match x.Item with
        | ClassVar (_, vis, _, _) -> vis
        | ClassProc (_, vis, _, _, _, _, _) -> vis

    member x.Name = match x.Item with
        | ClassVar (name, _, _, _) -> name
        | ClassProc (name, _, _, _, _, _, _) -> name


and InterfaceDecl =
    | InterfaceProc of (*name*) string * (*params*) list<Param> * (*returnType*) PType
    

and InterfaceDeclA(item:InterfaceDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member val PType = Undef with get, set

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


type TopDecl =
    | Using of string
    | Namespace of string * list<NamespaceDeclA>

type TopDeclA(item:TopDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item
    member x.IsNamespace = match item with
        | Namespace (name, decls) -> true
        | _ -> false


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
        | ClassProc (_, _, _, _, _, _, exprA) -> branchFunc cdecl [fAST exprA]

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
