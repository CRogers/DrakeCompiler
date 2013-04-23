module Tree

open Print
open LLVMTypes
open LLVM.Generated.Core
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open System.Diagnostics
open System
open Util

type RefType =
    | LocalRef
    | InstanceVarRef
    | StaticVarRef
    | InstanceProcRef
    | StaticProcRef
with
    override x.ToString() = fmt x

type Name = string
type InterfaceName = string

type IsStatic =
    | Static
    | NotStatic
    with override x.ToString() = fmt x

let isStatic x = match x with
    | Static -> true
    | NotStatic -> false

type IsStruct =
    | Struct
    | NotStruct
    with override x.ToString() = fmt x

type Visibility =
    | Private
    | Public
    with override x.ToString() = fmt x


type NPKey = 
    | VarKey of string
    | ProcKey of string * list<string>
    with override x.ToString() = fmt x

and PType = 
    | Undef
    | InitialType of string
    | ParamedType of PType * PType list
    | TypeParam of string
    | Type of NamespaceDeclA
    //| PFunc of (*arg types*) list<PType> * (*return type*) PType * IsStatic
    with override x.ToString() = match x with
        | Type nA -> sprintf "Type \"%s\"" nA.QName
        | ParamedType (ptype, params_) -> sprintf "ParamedType (%s, [%s])" (ptype.ToString()) <| Util.joinMap ", " (fun x -> x.ToString()) params_
        | _ -> fmt x

and Ref(name: string, ptype:PType, reftype:RefType) =
    member x.Name = name
    member val PType = ptype with get, set
    member val RefType = reftype
    member val ValueRef:option<ValueRef> = None with get, set

    override x.ToString() = sprintf "Ref(%s, %s, %s)" x.Name (x.PType.ToString()) (x.RefType.ToString())

and Pos(startPos:Position, endPos:Position) =
    member x.StartPos = startPos
    member x.EndPos = endPos
    static member NilPosition = {pos_fname= ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
    static member NilPos = Pos(Pos.NilPosition, Pos.NilPosition)
    override x.ToString() = "" //sprintf "s(%i,%i)e(%i,%i)" x.StartPos.Line x.StartPos.Column x.EndPos.Line x.EndPos.Column

and Param(name: string, ptype: PType) =
    member x.Name = name
    member val PType = ptype with get, set
    override x.ToString() = sprintf "%s:%s" name (x.PType.ToString())

and Constraint = Param

and TypeParamEnv = Map<string, PType>

and ITemplate = interface
    abstract TypeParams:list<string> with get, set
    abstract TypeConstraints:list<Constraint> with get, set
    abstract TypeEnv:TypeParamEnv with get, set
    end

and CIRef = 
    | ClassRef of ClassDeclA
    | InterfaceRef of InterfaceDeclA
    with override x.ToString() = match x with
        | ClassRef cA -> sprintf "ClassRef (%s)" (cA.ToString())
        | InterfaceRef iA -> sprintf "InterfaceRef (%s)" (iA.ToString()) 
    

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
 
and [<AbstractClass>] AnnotRefs<'k,'v when 'k:comparison>(pos:Pos) = 
    inherit Annot(pos)
    let mutable refs:Map<'k,'v> = Map.empty;

    member x.Refs = refs
    member x.AddRef(key, ref:'v) = refs <- refs.Add(key, ref)
    member x.AddRefs(refs:Map<'k,'v>) = Map.iter (fun key ref -> x.AddRef(key, ref)) refs
    member x.AddRefs(refs) = Seq.iter (fun ref -> x.AddRef(ref)) refs
    member x.GetRef(key) = Map.tryFind key refs


and Expr =
    | ConstInt of (*size*) int * (*value*) int64
    | ConstBool of bool
    | Var of string
    | VarStatic of PType ref
    | Dot of ExprA * string
    | DotTemplate of ExprA * string * list<PType> ref
    | DotInstance of ExprA * ClassDeclA
    | DotStatic of NamespaceDeclA * ClassDeclA
    | Binop of string * ExprA * ExprA
    | Cast of PType ref * ExprA
    | Call of ExprA * list<ExprA>
    | CallStatic of ClassDeclA * list<ExprA>
    | CallInstance of ClassDeclA * ExprA * list<ExprA>
    | CallVirtual of InterfaceDeclA * ExprA * list<ExprA>
    | Assign of ExprA * ExprA
    | DeclVar of string * (*Assign*) ExprA
    | Return of ExprA
    | ReturnVoid
    | If of ExprA * ExprA * ExprA
    | While of ExprA * ExprA
    | Seq of ExprA ref * ExprA ref
    | Nop

and ExprA(item:Expr, pos:Pos) =
    inherit AnnotRefs<string,Ref>(pos)    

    member val Item = item with get, set
    override x.ItemObj = x.Item :> obj

    member val PType = Undef with get, set

    override x.ToString() = 
        let union, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x.Item, x.Item.GetType())
        union.Name + match x.PType with
        | Undef -> ""
        | _ -> ":" + x.PType.ToString()


and ClassDecl =
    | ClassVar  of Name * Visibility * IsStatic * PType ref * ExprA
    | ClassProc of Name * Visibility * IsStatic * list<Param> ref * (*returnType*) PType ref * (*body*) ExprA
    with override x.ToString() = match x with
        | ClassVar  (n, vis, isStatic, ptype, _) -> sprintf "ClassVar (%s, %s, %s, %s)" n (vis.ToString()) (isStatic.ToString()) (ptype.ToString())
        | ClassProc (n, vis, isStatic, params_, retType, _) -> sprintf "ClassProc (%s, %s, %s, %s, %s)" n (vis.ToString()) (isStatic.ToString()) (listTS !params_) ((!retType).ToString())

and ClassDeclA(item:ClassDecl, pos:Pos) =
    inherit Annot(pos)
    member val Item = item with get, set
    override x.ItemObj = upcast x.Item

    member val Offset = -1 with get, set
    member val Ref = Ref("", Undef, StaticProcRef) with get, set
    member val FuncType:option<TypeRef> = None with get, set
    member val IsCtor = false with get, set
    member val IsBinop = false with get, set
    member val DefiningMethod:option<InterfaceDeclA> = None with get, set
    member val IfaceProcStub:option<ValueRef> = None with get, set

    interface ITemplate with
        member val TypeParams = [] with get, set
        member val TypeConstraints = [] with get, set
        member val TypeEnv = Map.empty with get, set

    member x.IsProc = match x.Item with
        | ClassVar _ -> false
        | ClassProc _ -> true

    member x.PType = match x.Item with
        | ClassVar (_, _, _, ptype, _) -> !ptype
        | ClassProc (_, _, _, _, returnType, _) -> !returnType

    member x.Visibility = match x.Item with
        | ClassVar (_, vis, _, _, _) -> vis
        | ClassProc (_, vis, _, _, _, _) -> vis

    member x.Name = match x.Item with
        | ClassVar (name, _, _, _, _) -> name
        | ClassProc (name, _, _, _, _, _) -> name

    member x.Params = match x.Item with
        | ClassProc (_, _, _, params_, _, _) -> !params_
        | _ -> failwithf "Can only get params for ClassProcs"

    member x.IsStatic = match x.Item with
        | ClassVar (_, _, isStatic, _, _) -> isStatic
        | ClassProc (_, _, isStatic, _, _, _) -> isStatic

    override x.ToString() = x.Item.ToString()


and InterfaceDecl =
    | InterfaceProc of (*name*) Name * (*params*) list<Param> * (*returnType*) PType ref
    with override x.ToString() = match x with
        | InterfaceProc (n, params_, returnType) -> sprintf "InterfaceProc (%s, %s, %s)" n (listTS params_) ((!returnType).ToString())
    

and InterfaceDeclA(item:InterfaceDecl, pos:Pos) =
    inherit Annot(pos)
    member x.Item = item
    override x.ItemObj = upcast item

    member val GlobalOffset = -1 with get, set
    member val FuncType:option<TypeRef> = None with get, set

    interface ITemplate with
        member val TypeParams = [] with get, set
        member val TypeConstraints = [] with get, set
        member val TypeEnv = Map.empty with get, set

    member x.PType = match x.Item with
        | InterfaceProc (_, params_, returnType) -> !returnType

    member x.Name = match x.Item with
        | InterfaceProc (name, _, _) -> name

    member x.Params = match x.Item with
        | InterfaceProc (_, params_, _) -> params_

    override x.ToString() = x.Item.ToString()


and NamespaceDecl =
    | Class of Name * Visibility * IsStruct * list<PType> ref * list<ClassDeclA>
    | Interface of Name * Visibility * list<PType> ref * list<InterfaceDeclA>
    with override x.ToString() =  match x with
        | Class     (n, vis, isS, ifaces, _) -> sprintf "Class (%s, %s, %s, %s)" n (vis.ToString()) (isS.ToString()) (listTS !ifaces)
        | Interface (n, vis, ifaces, _) -> sprintf "Interface (%s, %s, %s)" n (vis.ToString()) (listTS !ifaces)

and NamespaceDeclA(item:NamespaceDecl, pos:Pos) =
    inherit AnnotRefs<NPKey,CIRef>(pos)

    let bindPointerType t = Option.bind (fun t -> Some <| pointerType t 0u) t


    member val Item = item with get, set
    override x.ItemObj = upcast x.Item

    member x.CtorCA = match x.GetRef(ProcKey ("ctor", [])) with Some (ClassRef cA) -> cA

    member val AllInterfaces:list<NamespaceDeclA> = [] with get, set
    member val ImplementedBy:list<NamespaceDeclA> = [] with get, set

    member val InstanceType:option<TypeRef> = None with get, set
    member val StaticType:option<TypeRef> = None with get, set
    member val VTableType:option<TypeRef> = None with get, set

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.InstancePointerType:option<TypeRef> = bindPointerType x.InstanceType
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.StaticPointerType:option<TypeRef> = bindPointerType x.StaticType
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.VTablePointerType:option<TypeRef> = bindPointerType x.VTableType

    member val VTable:option<ValueRef> = None with get, set

    member val IsBuiltin = false with get, set

    interface ITemplate with
        member val TypeParams = [] with get, set
        member val TypeConstraints = [] with get, set
        member val TypeEnv = Map.empty with get, set

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

    member x.InterfacePTypes = match x.Item with
        | Class (_, _, _, ifaces, _) -> !ifaces
        | Interface (_, _, ifaces, _) -> !ifaces

    member x.Interfaces = List.map (fun ptype -> match ptype with Type nA -> nA) x.InterfacePTypes

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.InterfaceDeclAs = match x.Item with
        | Interface (_, _, _, iAs) -> iAs
        | _ -> failwithf "Can't get InterfaceDecls for anything but an interface" 

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.AllInterfaceProcs = concatMap (fun (nA:NamespaceDeclA) -> nA.InterfaceDeclAs) x.AllInterfaces

    override x.ToString() = x.Item.ToString()


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


let isNonExpandedTemplate (it:ITemplate) = it.TypeParams.Length > 0


let userTypeToString ptype = match ptype with
    | Type nA -> nA.QName
    | _ -> failwithf "Not a Type: %s" <| ptype.ToString()

let ptypeToNA ptype = match ptype with
    | Type nA -> nA
    | _ -> failwithf "Not a Type: %s" <| ptype.ToString()

let paramsToPtype (params_:list<Param>) =
    List.map (fun (p:Param) -> p.PType) params_

let paramsToPtypeString (params_:list<Param>) =
    List.map userTypeToString <| paramsToPtype params_

let qualifiedName namespace_ classInterfaceName (extraNames:seq<string>) =
    namespace_ + "::" + classInterfaceName + if Seq.isEmpty extraNames
        then ""
        else "." + String.Join(".", extraNames)

let isQualifiedName (name:string) = name.Contains("::")

let namePTypesKey name ptypes : NPKey = ProcKey (name, List.map userTypeToString ptypes)
let nameParamsKey name params_ : NPKey = ProcKey (name, paramsToPtypeString params_)
let interfaceNPKey (iA:InterfaceDeclA) = nameParamsKey iA.Name iA.Params
let classNPKey (cA:ClassDeclA) = match cA.Item with
    | ClassProc (name, _, _, params_, _, _) -> nameParamsKey name !params_
    | ClassVar (name, _, _, _, _) -> VarKey name

let NPKeyPretty npk = match npk with
    | VarKey s -> s
    | ProcKey (name, ptypeStrs) -> sprintf "%s(%s)" name <| System.String.Join(", ", ptypeStrs)

let ifaceSignaturePretty (iA:IDA) = sprintf "%s(%s)" iA.Name <| System.String.Join(", ", paramsToPtypeString iA.Params)

let cpPrefix x = "System::" + x 

type CommonPtype =
    | Unit
    | Int of int
    | Bool
    | String

let commonPtypeStr x =
    match x with
        | Unit   -> "Unit"
        | Int i  -> "Int" + i.ToString()
        | Bool   -> "Bool"
        | String -> "String"
    |> cpPrefix

let commonPtype globals x =
    commonPtypeStr x    
    |> fun x -> Map.find x globals
    |> Type


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

let getInterfaceDecls (nAs:seq<NamespaceDeclA>) =
    nAs
    |> Seq.map (fun (nA:NamespaceDeclA) -> match nA.Item with Interface (_, _, _, iAs) -> Some iAs | _ -> None)
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
type BinopStore = Map<NPKey, CIRef>

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
        | ConstInt _
        | ConstBool _
        | Var _
        | VarStatic _ -> leafFunc exprA
        | Dot (eA, name) -> bf1 eA
        | DotTemplate (eA, _, _) -> bf1 eA
        | DotStatic (nA, cA) -> leafFunc exprA
        | DotInstance (eA, cA) -> bf1 eA
        | Binop (n, l, r) -> bf [l; r]
        | Cast (ptype, eA) -> bf1 eA
        | Call (feA, exprAs) -> bf <| feA :: exprAs
        | CallStatic (cA, eAs) -> bf eAs
        | CallInstance (cA, feA, eAs) -> bf (feA :: eAs)
        | CallVirtual (cA, feA, eAs) -> bf (feA :: eAs)
        | Assign (lvalue, innerExprA) -> bf [lvalue; innerExprA]
        | DeclVar (name, assignA) -> bf1 assignA
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