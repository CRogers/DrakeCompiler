module Tree

type Op = Add | Sub | Mul | Div

type Expr =
    | Int of int
    | Ident of string
    | Binop of Op * Expr * Expr

type Stmt =
    | Print of Expr

type Program = list<Stmt>