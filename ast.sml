structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | Eq | And | Equals | Or | Xor 
                | Implies | Lessthan | Greaterthan 
datatype other = Not | Negate 


datatype decl = ValDecl of id * exp

and exp = NumExp of int
        | NExp of other * exp
        | ifExp of exp * exp *  exp 
        | VarExp of id 
        | BinExp of binop * exp * exp
        | LetExp of decl * exp
        | BoolExp of bool

datatype value = IntVal of int
               | StringVal of string
               | BoolVal of bool
type environment = (id * value) list


fun envAdd(var:id, v:value, env:environment) = 
        (var,v)::env

fun envLookup (var:id, env:environment) = 
        case List.find(fn(x, _) => x = var) env of
           SOME (x,v ) => v
         | NONE => raise Fail "Environment lookup error"
end