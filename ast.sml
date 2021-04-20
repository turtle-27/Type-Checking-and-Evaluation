structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | Div | Eq

datatype decl = ValDecl of id*exp

and exp = NumExp of int
        | StringExp of string
        | VarExp of id
        | BinExp of binop*exp*exp
        | LetExp of decl*exp
end