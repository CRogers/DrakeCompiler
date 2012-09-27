module Tree

type Op = Add | Sub | Mul | Div

type Expr =
    | Int of int
    | Ident of string
    | Binop of Op * Expr * Expr
    | Call of string * list<Expr>
    | Var of string

type Stmt =
    | Print of Expr
    | Return of Expr

type Decl = 
    | Proc of (*name*) string * (*params*) list<string> * list<Stmt>

type Program = list<Decl>