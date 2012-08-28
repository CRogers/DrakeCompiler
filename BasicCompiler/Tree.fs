module Tree

type Op = Add | Sub | Mul | Div

type Expr =
    | Int of int
    | Binop of Op * Expr * Expr