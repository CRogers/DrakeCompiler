module Tree

open LLVM.Generated.Core
open System.Collections.Generic

type Op = Add | Sub | Mul | Div

type Expr =
    | Int of int
    | Binop of Op * Expr * Expr
    | Call of string * list<Expr>
    | Var of string

type Stmt =
    | Print of Expr
    | Assign of string * Expr
    | Return of Expr

type Decl = 
    | Proc of (*name*) string * (*params*) list<string> * list<Stmt>

type Program = list<Decl>

type Func(name: string, func: ValueRef, params: Map<string, ValueRef>) = class
    member x.Name = name
    member x.Func = func
    member x.Params = params
    end

type Environ(enclosingFunc: Func) = class
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

    end