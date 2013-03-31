module Tree

open Print
open LLVMTypes
open LLVM.Generated.Core
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open System
open Util

type RefType =
    | LocalRef
    | InstanceVarRef
    | InstanceProcRef
    | StaticProcRef
with
    override x.ToString() = fmt x

type Name = string
type InterfaceName = string

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


type PType = 
    | Undef
    | UserType of string
    | StaticType of NamespaceDeclA
    | PFunc of (*arg types*) list<PType> * (*return type*) PType
    | RefType of Ref
    with
    override x.ToString() = match x with
        | StaticType nA -> nA.QName
        | _ -> fmt x

and Ref(name: string, ptype:PType, reftype:RefType) =
    member x.Name = name
    member val PType = ptype with get, set
    member val RefType = reftype
    member val ValueRef = uninitValueRef with get, set
    member x.IsUninitialised = isUninitValueRef x.ValueRef

    override x.ToString() = sprintf "Ref(%s, %s, %s)" x.Name (x.PType.ToString()) (x.RefType.ToString())

and Pos(startPos:Position, endPos:Position) =
    member x.StartPos = startPos
    member x.EndPos = endPos
    override x.ToString() = "" //sprintf "s(%i,%i)e(%i,%i)" x.StartPos.Line x.StartPos.Column x.EndPos.Line x.EndPos.Column
    static member NilPosition = {pos_fname= ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
    static member NilPos = Pos(Pos.NilPosition, Pos.NilPosition)

and Param(name: string, ptype: PType) =
    member x.Name = name
    member val PType = ptype with get, set
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)


and CIRef = 
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
    | Dot of ExprA * string
    | Binop of string * ExprA * ExprA
    | Call of ExprA * list<ExprA>
    | Assign of ExprA * ExprA
    | DeclVar of string * (*Assign*) ExprA
    | Print of ExprA
    | Return of ExprA
    | ReturnVoid
    | If of ExprA * ExprA * ExprA
    | While of ExprA * ExprA
    | Seq of ExprA ref * ExprA ref
    | Nop

and ExprA(item:Expr, pos:Pos) =
    inherit AnnotRefs<Ref>(pos)    

    member val Item = item with get, set
    override x.ItemObj = x.Item :> obj

    member val PType = Undef with get, set

    override x.ToString() = base.ToString() + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType


and ClassDecl =
    | ClassVar of Name * Visibility * IsStatic * PType ref * ExprA
    | ClassProc of Name * Visibility * IsStatic * list<Param> ref * (*returnType*) PType ref * (*body*) ExprA
    

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member val Item = item with get, set
    override x.ItemObj = upcast x.Item

    member val Offset = -1 with get, set
    member val Ref = Ref("", Undef, StaticProcRef) with get, set
    member val FuncType = new TypeRef(nativeint 0xDEF) with get, set

    member val EnclosingNDA:option<NamespaceDeclA> = None with get, set

    member x.IsProc = match x.Item with
        | ClassVar _ -> false
        | ClassProc _ -> true

    member x.PType = match x.Item with
        | ClassVar (_, _, _, ptype, _) -> !ptype
        | ClassProc (_, _, _, params_, returnType, _) ->
            let ptypeParams = List.map (fun (p:Param) -> p.PType) !params_
            PFunc (ptypeParams, !returnType)

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

    member val EnclosingNDA:option<NamespaceDeclA> = None with get, set

    member x.PType = match x.Item with
        | InterfaceProc (_, params_, returnType) ->
            let ptypeParams = List.map (fun (p:Param) -> p.PType) params_
            PFunc (ptypeParams, !returnType)

    member x.Name = match x.Item with
        | InterfaceProc (name, _, _) -> name

    member x.Params = match x.Item with
        | InterfaceProc (_, params_, _) -> params_

and NamespaceDecl =
    | Class of Name * Visibility * IsStruct * list<InterfaceName> ref * list<ClassDeclA>
    | Interface of Name * Visibility * list<InterfaceName> ref * list<InterfaceDeclA>

and NamespaceDeclA(item:NamespaceDecl, pos:Pos) =
    inherit AnnotRefs<CIRef>(pos)

    let bindPointerType t = Option.bind (fun t -> Some <| pointerType t 0u) t

    member x.Item:NamespaceDecl = item
    override x.ItemObj = upcast item

    member val CtorRef:Ref = Ref(null, Undef, StaticProcRef) with get, set
    member val Interfaces:list<NamespaceDeclA> = [] with get, set
    member val AllInterfaces:list<NamespaceDeclA> = [] with get, set
    member val ImplementedBy:list<NamespaceDeclA> = [] with get, set

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
            | Class (_, vis, _, _, _) -> vis
            | Interface (_, vis, _, _) -> vis

    member x.Name = match x.Item with
        | Class (name, _, _, _, _) -> name
        | Interface (name, _, _, _) -> name

    member x.IsStruct = match x.Item with
        | Class (_, _, isStruct, _, _) -> isStruct
        | Interface (_, _, _, _) -> NotStruct

    member x.InterfaceNames = match x.Item with
        | Class (_, _, _, ifaces, _) -> !ifaces
        | Interface (_, _, ifaces, _) -> !ifaces

    member x.InterfaceDeclAs = match x.Item with
        | Interface (_, _, _, iAs) -> iAs
        | _ -> failwithf "Can't " 

    member x.AllInterfaceProcs = concatMap (fun (nA:NamespaceDeclA) -> nA.InterfaceDeclAs) x.AllInterfaces


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


type CDA = ClassDeclA
type IDA = InterfaceDeclA
type NDA = NamespaceDeclA



let userTypeToString ptype = match ptype with
    | UserType s -> s
    | _ -> failwithf "Not a UserType"

let paramsToPtype (params_:list<Param>) =
    List.map (fun (p:Param) -> p.PType) params_

let paramsToPtypeString (params_:list<Param>) =
    List.map userTypeToString <| paramsToPtype params_

let paramsReturnTypeToPtype (params_:list<Param>) returnType =
    PFunc (paramsToPtype params_, returnType)

let qualifiedName namespace_ classInterfaceName (extraNames:seq<string>) =
    namespace_ + "::" + classInterfaceName + if Seq.isEmpty extraNames
        then ""
        else "." + String.Join(".", extraNames)

let isQualifiedName (name:string) = name.Contains("::")

type NPKey = string * list<PType>
let nameParamsKey name params_ : NPKey = (name, paramsToPtype params_)
let interfaceNPKey (iA:InterfaceDeclA) = nameParamsKey iA.Name iA.Params
let classNPKey (cA:ClassDeclA) = match cA.Item with
    | ClassProc (name, _, _, params_, _, _) -> nameParamsKey name !params_
    | _ -> failwithf "Must call on ClassProc only"

let cpPrefix x = "System::" + x 

type CommonPtype =
    | Unit
    | Int of int
    | Bool
    | String

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


let getNamespaceDecls (program:Program) =
    Util.concatMap (Seq.map (fun (tdA:TopDeclA) -> match tdA.Item with Namespace (_, nAs) -> Some nAs | _ -> None)) program
    |> Util.getSomes
    |> Seq.concat

let getClassDecls (nAs:seq<NamespaceDeclA>) =
    nAs
    |> Seq.map (fun (nA:NamespaceDeclA) -> match nA.Item with Class (_, _, _, _, cAs) -> Some cAs | _ -> None)
    |> Util.getSomes
    |> Seq.concat

let getAllExprs (nAs:seq<NamespaceDeclA>) =
    getClassDecls nAs
    |> Seq.map (fun (cA:ClassDeclA) -> match cA.Item with
        | ClassProc (_, _, _, _, _, eA) -> eA
        | ClassVar (_, _, _, _, eA) -> eA)

let isReturn (eA:ExprA) = match eA.Item with
    | Return _ -> true
    | ReturnVoid -> true
    | _ -> false

let isReturnVoid (eA:ExprA) = match eA.Item with
    | ReturnVoid -> true
    | _ -> false

let isReturnTyped (eA:ExprA) = match eA.Item with
    | Return _ -> true
    | _ -> false

type FoundInSeq<'a> =
    | NotFound
    | FoundInMiddle of 'a * 'a
    | FoundAtEnd of 'a

let rec findInSeq pred (eA:ExprA) = match eA.Item with
    | Seq (e1A, e2A) -> if pred !e1A then FoundInMiddle (eA, !e1A) else findInSeq pred !e2A
    | _ -> if pred eA then FoundAtEnd eA else NotFound

let rec lastInSeq (expr:ExprA) = match expr.Item with
    | Seq (e1A, e2A) -> lastInSeq !e2A
    | _ -> expr

let rec lastInSeqAndPrev (eA:ExprA) = 
    let rec lisap last (eA:ExprA) =        
        match eA.Item with
            | Seq (e1A, e2A) -> lisap (Some eA) !e2A
            | _ -> (last, eA)
    
    lisap None eA

let isLastInSeqRet eA = isReturn <| lastInSeq eA

type GlobalStore = Map<string, NamespaceDeclA>
type BinopStore = Map<Name * list<string>, list<ClassDeclA>>

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
        | Dot (eA, name) -> bf1 eA
        | Binop (n, l, r) -> bf [l; r]
        | Call (feA, exprAs) -> bf <| feA :: exprAs
        | Assign (lvalue, innerExprA) -> bf [lvalue; innerExprA]
        | DeclVar (name, assignA) -> bf1 assignA
        | Print e -> bf1 e
        | Return e -> bf1 e
        | ReturnVoid -> bf []
        | If (test, then_, else_) -> bf [test; then_; else_]
        | While (test, body) -> bf [test; body]
        | Seq (e1A, e2A) -> bf [!e1A; !e2A]
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
        | Class (_, _, _, _, cdeclAs) -> fAST foldASTClassDecl cdeclAs
        | Interface (_, _, _, ideclAs) -> fAST foldASTInterfaceDecl ideclAs

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
