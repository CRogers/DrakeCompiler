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

type PType = Undef | Decl | Unit | Int of int | Bool | UserType of string | PFunc of PType * PType
    with
    override x.ToString() = fmt x
    member x.IsFunc = match x with
        | PFunc _ -> true
        | _ -> false
    member x.NextArg = match x with
        | PFunc (x, y) -> x
        | _ -> failwithf "%s is not of type PFunc!" (x.ToString())
    member x.FollowingArg = match x with
        | PFunc (x, y) -> y
        | _ -> failwithf "%s is not of type PFunc!" (x.ToString())
    member x.ConsumeArgs(argPtypes) =
        argPtypes
        |> Seq.fold (fun resFuncPt argPt ->
            if Option.isNone resFuncPt then None else (
                let funcPt:PType = Option.get resFuncPt
                if funcPt.IsFunc && argPt = funcPt.NextArg then Some (funcPt.FollowingArg) else None)) (Some x)


let parsePType str = match str with
    | "Unit" -> Unit
    | "Int" -> Int 32
    | "Bool" -> Bool

type Pos(startPos:Position, endPos:Position) =
    member x.StartPos = startPos
    member x.EndPos = endPos
    override x.ToString() = sprintf "s(%i,%i)e(%i,%i)" x.StartPos.Line x.StartPos.Column x.EndPos.Line x.EndPos.Column

type RefType = Local | Parameter | Global

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

let qualifiedName namespace_ (names:list<string>) = namespace_ + "." + String.Join(".", names)

[<AbstractClass>]
type Annot(pos:Pos) =
    let mutable refs:Map<string,Ref> = Map.empty;
    let filterRefs refType =
        Map.filter (fun k (v:Ref) -> v.RefType = refType) refs
        |> Map.toSeq |> Seq.map (fun (k,v) -> v)

    member x.Refs = refs
    member x.AddRef(ref:Ref) = refs <- refs.Add(ref.Name, ref)
    member x.AddRefs(refs) = Map.iter (fun name ref -> x.AddRef(ref)) refs
    member x.GetRef(name) =
        try Map.find name refs
        with KeyNotFoundException -> x.Globals.[name]
    member x.LocalRefs with get () = filterRefs Local
    member x.ParamRefs with get () = filterRefs Parameter

    member val Globals:Dictionary<string,Ref> = null with get, set
    member x.AddGlobal(ref:Ref) =
        if x.Globals.ContainsKey(ref.Name) then
            failwithf "Already have the global ref %s" ref.Name
        else
            x.Globals.[ref.Name] <- ref

    member val CIGlobals:Dictionary<string,Dictionary<string,CIRef>> = null with get, set

    member x.AddCIGlobal(namespace_:string, name:string, ndeclA:NamespaceDeclA) =
        let qname = qualifiedName namespace_ [name]
        if not <| x.CIGlobals.ContainsKey(namespace_) then
            x.CIGlobals.[namespace_] <- new Dictionary<string,CIRef>()
        let subdict = x.CIGlobals.[namespace_]
        
        if subdict.ContainsKey(name) then
            failwithf "Already have the ClassIntefaceGlobal ref %s" qname
        else
            subdict.[name] <- CIRef(qname, ndeclA)

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

and CIRef(namespace_:string, name:string, decl:NamespaceDeclA) =
    member x.Namespace = namespace_
    member x.Name = name
    member x.QName = qualifiedName namespace_ [name]
    member x.Decl = decl
        

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
    with
    interface IVisibility with
        member x.Visibility = match x with
            | ClassVar (_, vis, _, _) -> vis
            | ClassProc (_, vis, _, _, _) -> vis

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item


and InterfaceDecl =
    | InterfaceProc of (*name*) string * (*params*) list<Param> * (*returnType*) PType
    with
    interface IVisibility with
        member x.Visibility = Public

and InterfaceDeclA(item:InterfaceDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item


and NamespaceDecl =
    | Class of string * Visibility * list<ClassDeclA>
    | Interface of string * Visibility * list<InterfaceDeclA>
    with
    interface IVisibility with
        member x.Visibility = match x with
            | Class (_, vis, _) -> vis
            | Interface (_, vis, _) -> vis

and NamespaceDeclA(item:NamespaceDecl, pos:Pos) = 
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item


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
