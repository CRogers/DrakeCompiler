module Tree

open LLVM.Generated.Core

type Op = Add | Sub | Mul | Div

type Expr =
    | Int of int
    | Binop of Op * Expr * Expr
    | Call of string * list<Expr>
    | Var of string

type Stmt =
    | Print of Expr
    | Return of Expr

type Decl = 
    | Proc of (*name*) string * (*params*) list<string> * list<Stmt>

type Program = list<Decl>

type Func(name: string, func: ValueRef, params: Map<string, ValueRef>) = class
    member x.Name = name
    member x.Func = func
    member x.Params = params
    end