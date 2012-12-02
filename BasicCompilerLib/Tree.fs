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

type PType = Undef | Unit | Int | Bool with
    override x.ToString() = fmt x

let parsePType str = match str with
    | "Unit" -> Unit
    | "Int" -> Int
    | "Bool" -> Bool

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
    override x.ToString() = sprintf "%s:%s" x.Name (fmt x.PType)

type SRP = KeyValuePair<string, Ref>

type Annot<'a>(item:'a, pos:Pos) =
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

    member x.Pos = pos
    member x.Item = item
    member val PType = Undef with get, set

    override x.ToString() = fmt x.Item + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType
        (*":" + x.Pos.ToString() +
        ":" + fmt (List.ofSeq <| Seq.map (fun (kvp:KeyValuePair<string,Ref>) -> kvp.Value) x.Refs)*)
        

type Expr =
    | ConstInt of int
    | ConstBool of bool
    | Binop of Op * ExprA * ExprA
    | Call of string * list<ExprA>
    | Var of string
    | Assign of string * ExprA
    | Print of ExprA
    | DeclVar of string * (*Assign*) ExprA
    | Return of ExprA
    | If of ExprA * ExprA * ExprA
    | While of ExprA * ExprA
    | Seq of ExprA * ExprA

and ExprA = Annot<Expr>

type Param(name: string, ptype: PType) =
    member x.Name = name
    member x.PType = ptype
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)

type Decl = 
    | Proc of (*name*) string * (*params*) list<Param> * (*returnType*) PType * ExprA

type DeclA = Annot<Decl>

type Program = list<DeclA>

type Func(name: string, func: ValueRef, params: Map<string, ValueRef>) =
    member x.Name = name
    member x.Func = func
    member x.Params = params

type Environ(module_: ModuleRef, enclosingFunc: Func) =
    let refs = new Dictionary<string, ValueRef>()
    let icount = new Dictionary<string, int>()

    do
        Map.iter (fun k v -> refs.Add(k, v)) enclosingFunc.Params

    member x.Module = module_
    member x.EnclosingFunc = enclosingFunc
    member x.AddRef(name, vr) = refs.[name] <- vr
    member x.GetRef(name) = refs.[name]
    
    member x.GetName name =
        if not <| icount.ContainsKey(name) then
            icount.[name] <- 0
        let ret = icount.[name]
        icount.[name] <- ret + 1
        name + "." + ret.ToString()
    member x.GetTmp() = x.GetName "tmp"