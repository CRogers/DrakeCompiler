module Tree

open Print
open LLVM.Generated.Core
open System.Collections.Generic
open System

type Op = 
    | Add | Sub | Mul | Div
    | And | Or | Not

type PType = Undef | Unit | Int | Bool

let parsePType str = match str with
    | "Unit" -> Unit
    | "Int" -> Int
    | "Bool" -> Bool

type Expr =
    | ConstInt of int
    | ConstBool of bool
    | Binop of Op * ExprA * ExprA
    | Call of string * list<ExprA>
    | Var of string

and ExprA(expr: Expr) =     
    member x.Expr = expr
    member val PType = Undef with get, set
    override x.ToString() = fmt expr + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType

type Stmt =
    | Print of ExprA
    | Assign of string * ExprA
    | Return of ExprA

and StmtA(stmt: Stmt) =
    member x.Stmt = stmt
    member val PType = Undef with get, set
    override x.ToString() = fmt stmt + match x.PType with
        | Undef -> ""
        | _ -> ":" + fmt x.PType

type Param(name: string, ptype: PType) =
    member x.Name = name
    member x.PType = ptype
    override x.ToString() = sprintf "%s:%s" name (fmt x.PType)

type Decl = 
    | Proc of (*name*) string * (*params*) list<Param> * (*returnType*) PType * list<StmtA>

and DeclA(decl: Decl) =
    member x.Decl = decl
    member val PType = Undef with get, set
    override x.ToString() = fmt decl

type Program = list<DeclA>

type Func(name: string, func: ValueRef, params: Map<string, ValueRef>) =
    member x.Name = name
    member x.Func = func
    member x.Params = params

type Environ(enclosingFunc: Func) =
    let refs = new Dictionary<string, ValueRef>()
    let icount = new Dictionary<string, int>()

    do
        Map.iter (fun k v -> refs.Add(k, v)) enclosingFunc.Params

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