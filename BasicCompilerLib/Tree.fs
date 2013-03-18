module Tree

open Print
open LLVMTypes
open LLVM.Generated.Core
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open System
open Util

type Op = 
    | Add | Sub | Mul | Div
    | BoolAnd | BoolOr | Not
    | Lt | Gt | LtEq | GtEq | Eq

type RefType =
    | LocalRef
    | InstanceVarRef
    | InstanceProcRef
    | StaticProcRef
with
    override x.ToString() = fmt x

type PType = 
    | Undef
    | UserType of string
    | StaticType of string
    | PFunc of (*arg types*) list<PType> * (*return type*) PType
    | RefType of Ref
    with
    override x.ToString() = fmt x

and Ref(name: string, ptype:PType, reftype:RefType) =
    member x.Name = name
    member val PType = ptype with get, set
    member val RefType = reftype
    member val ValueRef = uninitValueRef with get, set
    member x.IsUninitialised = isUninitValueRef x.ValueRef

    override x.ToString() = sprintf "Ref(%s, %s, %s)" x.Name (x.PType.ToString()) (x.RefType.ToString())

type CommonPtype =
    | Unit
    | Int of int
    | Bool
    | String

type Pos(startPos:Position, endPos:Position) =
    member x.StartPos = startPos
    member x.EndPos = endPos
    override x.ToString() = sprintf "s(%i,%i)e(%i,%i)" x.StartPos.Line x.StartPos.Column x.EndPos.Line x.EndPos.Column
    static member NilPosition = {pos_fname= ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
    static member NilPos = Pos(Pos.NilPosition, Pos.NilPosition)

type Param(name: string, ptype: PType) =
    member x.Name = name
    member val PType = ptype with get, set
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)




let qualifiedName namespace_ classInterfaceName (extraNames:seq<string>) =
    namespace_ + "::" + classInterfaceName + if Seq.isEmpty extraNames
        then ""
        else "." + String.Join(".", extraNames)

let isQualifiedName (name:string) = name.Contains("::")

let paramsReturnTypeToPtype (params_:list<Param>) returnType =
    let ptypeParams = List.map (fun (p:Param) -> p.PType) params_
    PFunc (ptypeParams, returnType)

let cpPrefix x = "System::" + x 

let commonPtype x = UserType (cpPrefix <| match x with
    | Unit   -> "Unit"
    | Int i  -> "Int" + i.ToString()
    | Bool   -> "Bool"
    | String -> "String")


let getGlobal globals namespace_ (usings:seq<string>) name =
    match Map.tryFind name globals with
        | Some nA -> Some nA
        | None ->
            let namespaceLocal = namespace_ + "::" + name
            match Map.tryFind namespaceLocal globals with
                | Some nA -> Some nA
                | None ->
                    // Cycle through usings
                    let us = Seq.map (fun using ->
                        Map.tryFind (using + "::" + name) globals) usings
                    match Seq.tryFind Option.isSome us with
                        | Some snA -> snA
                        | None -> None


type Name = string

type IsStatic =
    | Static
    | NotStatic

let isStatic x = match x with
    | Static -> true
    | NotStatic -> false

type IsStruct =
    | Struct
    | NotStruct

type Visibility =
    | Private
    | Public

type IVisibility =
    abstract Visibility : Visibility

type CIRef = 
    | ClassRef of ClassDeclA
    | InterfaceRef of InterfaceDeclA
with
    override x.ToString() = fmt x
    

and [<AbstractClass>] Annot(pos:Pos) =
    member val LocalVars:list<Ref> = [] with get, set
    member x.AddLocalVar(ref) = x.LocalVars <- ref :: x.LocalVars
    member x.AddLocalVars(refs) = x.LocalVars <- refs @ x.LocalVars

    member x.Pos = pos
    abstract member ItemObj : obj
    member x.ItemAs<'a>():'a = downcast x.ItemObj

    member val QName = "" with get, set
    member val Usings:list<string> = [] with get, set
    member val Namespace = "" with get, set
    member val NamespaceDecl:Option<NamespaceDeclA> = None with get, set

    override x.ToString() = fmt x.ItemObj
        (*":" + x.Pos.ToString() +
        ":" + fmt (List.ofSeq <| Seq.map (fun (kvp:KeyValuePair<string,Ref>) -> kvp.Value) x.Refs)*)
 
and [<AbstractClass>] AnnotRefs<'a>(pos:Pos) = 
    inherit Annot(pos)
    let mutable refs:Map<string,'a> = Map.empty;

    member x.Refs = refs
    member x.AddRef(name, ref:'a) = refs <- refs.Add(name, ref)
    member x.AddRefs(refs:Map<string,'a>) = Map.iter (fun name ref -> x.AddRef(name, ref)) refs
    member x.AddRefs(refs) = Seq.iter (fun ref -> x.AddRef(ref)) refs
    member x.GetRef(name) = Map.tryFind name refs


and Expr =
    | ConstInt of (*size*) int * (*value*) int64
    | ConstBool of bool
    | ConstUnit
    | Var of string
    | Binop of Op * ExprA * ExprA
    | Dot of ExprA * string
    | Call of ExprA * list<ExprA>
    | Assign of ExprA * ExprA
    | DeclVar of string * (*Assign*) ExprA
    | Print of ExprA
    | Return of ExprA
    | ReturnVoid
    | If of ExprA * ExprA * ExprA
    | While of ExprA * ExprA
    | Seq of ExprA * ExprA
    | Nop

and ExprA(item:Expr, pos:Pos) =
    inherit AnnotRefs<Ref>(pos)    

    member x.Item = item
    override x.ItemObj = item :> obj

    member val PType = Undef with get, set

    override x.ToString() = base.ToString() + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType


and ClassDecl =
    | ClassVar of Name * Visibility * IsStatic * PType ref * ExprA
    | ClassProc of Name * Visibility * IsStatic * list<Param> ref * (*returnType*) PType ref * (*body*) ExprA
    

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member val Offset = -1 with get, set
    member val Ref = Ref("", Undef, StaticProcRef) with get, set
    member val FuncType = new TypeRef(nativeint 0xDEF) with get, set

    member x.IsProc = match x.Item with
        | ClassVar _ -> false
        | ClassProc _ -> true

    member x.PType = match x.Item with
        | ClassVar (_, _, _, ptype, _) -> !ptype
        | ClassProc (_, _, _, params_, returnType, _) -> paramsReturnTypeToPtype !params_ !returnType

    member x.Visibility = match x.Item with
        | ClassVar (_, vis, _, _, _) -> vis
        | ClassProc (_, vis, _, _, _, _) -> vis

    member x.Name = match x.Item with
        | ClassVar (name, _, _, _, _) -> name
        | ClassProc (name, _, _, _, _, _) -> name

    member x.IsStatic = match x.Item with
        | ClassVar (_, _, isStatic, _, _) -> isStatic
        | ClassProc (_, _, isStatic, _, _, _) -> isStatic


and InterfaceDecl =
    | InterfaceProc of (*name*) Name * (*params*) list<Param> * (*returnType*) PType ref
    

and InterfaceDeclA(item:InterfaceDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member x.PType = match x.Item with
        | InterfaceProc (_, params_, returnType) -> !returnType

    member x.Name = match x.Item with
        | InterfaceProc (name, _, _) -> name

and NamespaceDecl =
    | Class of Name * Visibility * IsStruct * list<ClassDeclA>
    | Interface of Name * Visibility * list<InterfaceDeclA>

and NamespaceDeclA(item:NamespaceDecl, pos:Pos) =
    inherit AnnotRefs<CIRef>(pos)

    let bindPointerType t = Option.bind (fun t -> Some <| pointerType t 0u) t

    member x.Item:NamespaceDecl = item
    override x.ItemObj = upcast item

    member val CtorRef:Ref = Ref(null, Undef, StaticProcRef) with get, set

    member val InstanceType:option<TypeRef> = None with get, set
    member val StaticType:option<TypeRef> = None with get, set
    member val VTableType:option<TypeRef> = None with get, set

    member x.InstancePointerType:option<TypeRef> = bindPointerType x.InstanceType
    member x.StaticPointerType:option<TypeRef> = bindPointerType x.StaticType
    member x.VTablePointerType:option<TypeRef> = bindPointerType x.VTableType

    member val IsBuiltin = false with get, set
    member val BuiltinCreator = (fun () -> ()) with get, set

    member x.IsClass = match x.Item with
        | Class _ -> true
        | Interface _ -> false

    member x.Visibility = match x.Item with
            | Class (_, vis, _, _) -> vis
            | Interface (_, vis, _) -> vis

    member x.Name = match x.Item with
        | Class (name, _, _, _) -> name
        | Interface (name, _, _) -> name

    member x.IsStruct = match x.Item with
        | Class (_, _, isStruct, _) -> isStruct
        | Interface (_, _, _) -> NotStruct


type TopDecl =
    | Using of Name
    | Namespace of Name * list<NamespaceDeclA>

type TopDeclA(item:TopDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item
    member x.IsNamespace = match item with
        | Namespace (name, decls) -> true
        | _ -> false


type CompilationUnit = list<TopDeclA>
type Program = list<CompilationUnit>

let getNamespaceDecls (program:Program) =
    Util.concatMap (Seq.map (fun (tdA:TopDeclA) -> match tdA.Item with Namespace (_, nAs) -> Some nAs | _ -> None)) program
    |> Util.getSomes
    |> Seq.concat

let getClassDecls (nAs:seq<NamespaceDeclA>) =
    nAs
    |> Seq.map (fun (nA:NamespaceDeclA) -> match nA.Item with Class (_, _, _, cAs) -> Some cAs | _ -> None)
    |> Util.getSomes
    |> Seq.concat

let isReturn (eA:ExprA) = match eA.Item with
    | Return _ -> true
    | ReturnVoid -> true
    | _ -> false

let rec lastInSeq (expr:ExprA) = match expr.Item with
    | Seq (e1A, e2A) -> lastInSeq e2A
    | _ -> expr

let isLastInSeqRet eA = isReturn <| lastInSeq eA

type GlobalStore = Map<string, NamespaceDeclA>

type Func(name: string, func: ValueRef, params_: Map<string, ValueRef>) =
    member x.Name = name
    member x.Func = func
    member x.Params = params_

type Environ(module_: ModuleRef, enclosingFunc: Ref) =
    member x.Module = module_
    member x.EnclosingFunc = enclosingFunc




let rec foldASTExpr (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (exprA:ExprA) =
    let fAST e = foldASTExpr branchFunc leafFunc e
    let bf1 e = branchFunc exprA <| [fAST e]
    let bf es = branchFunc exprA <| List.map fAST es
    match exprA.Item with
        | ConstInt _ -> leafFunc exprA
        | ConstBool _ -> leafFunc exprA
        | ConstUnit -> leafFunc exprA
        | Var n -> leafFunc exprA
        | Binop (op, l, r) -> bf [l; r]
        | Dot (eA, name) -> bf1 eA
        | Call (feA, exprAs) -> bf <| feA :: exprAs
        | Assign (lvalue, innerExprA) -> bf [lvalue; innerExprA]
        | DeclVar (name, assignA) -> bf1 assignA
        | Print e -> bf1 e
        | Return e -> bf1 e
        | ReturnVoid -> bf []
        | If (test, then_, else_) -> bf [test; then_; else_]
        | While (test, body) -> bf [test; body]
        | Seq (e1A, e2A) -> bf [e1A; e2A]
        | Nop -> branchFunc exprA []

let foldASTClassDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (cdecl:ClassDeclA) =
    let fAST e = foldASTExpr branchFunc leafFunc e
    match cdecl.Item with
        | ClassVar (_, _, _, _, exprA) -> branchFunc cdecl [fAST exprA]
        | ClassProc (_, _, _, _, _, exprA) -> branchFunc cdecl [fAST exprA]

let foldASTInterfaceDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (idecl:InterfaceDeclA) =
    leafFunc idecl

let foldASTNamespaceDecl (branchFunc:Annot -> list<'a> -> 'a)  (leafFunc:Annot -> 'a) (ndecl:NamespaceDeclA) =
    let fAST fASTFunc declAs = branchFunc ndecl <| List.map (fASTFunc branchFunc leafFunc) declAs
    match ndecl.Item with
        | Class (_, _, _, cdeclAs) -> fAST foldASTClassDecl cdeclAs
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
