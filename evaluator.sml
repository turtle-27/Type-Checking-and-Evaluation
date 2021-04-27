structure EVALUATOR =
struct
open AST
exception errorInEvalExp;
exception errorInEvalBinExp;
exception errorInEvalNExp;
exception errorInEvalIfExp;
val brokenTypes = Fail "Error in evaluation!"
fun evalExpList(e: exp list, env: environment): value list = 
    case e of
        [] => raise brokenTypes
    |   h::t => if t = [] then [evalExp(h, env)] else evalExpList(t,env)@[evalExp(h, env)]       
and 
evalExp(e:exp, env:environment):value = 
    case e of
        NumExp i => IntVal i 
    |   NExp (ot, e1) => evalNExp(ot, e1, env)
    |   ifExp (e1,e2,e3) => evalifExp(e1, e2, e3, env)
    |   VarExp x => envLookup (x, env)
    |   BinExp (b, e1, e2) => evalBinExp(b, e1, e2, env)
    |   LetExp(ValDecl(x, e1), e2) => 
        let 
            val v1 = evalExp (e1, env)
        in 
            evalExp(e2, envAdd(x, v1, env))
        end
    |   BoolExp b =>  BoolVal b
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env)) of 
          (Add, IntVal i1, IntVal i2) => IntVal(i1+i2)
        | (Sub, IntVal i1, IntVal i2) => IntVal(i1-i2)
        | (Mul, IntVal i1, IntVal i2) => IntVal(i1*i2)
        | (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
        | (Equals, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
        | (And, BoolVal b1, BoolVal b2)  => BoolVal (b1 andalso b2)
        | (Or, BoolVal b1, BoolVal b2)  => BoolVal (b1 orelse b2)
        | (Xor, BoolVal b1, BoolVal b2) => BoolVal ((b1 orelse b2) andalso (not b1 orelse not b2))
        | (Implies, BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2)
        | (Lessthan, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
        | (Greaterthan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
        |  _ => raise errorInEvalBinExp
and evalNExp(ot: other, e1:exp, env:environment):value =
    case (ot, evalExp(e1, env)) of
        (Not, BoolVal b) => BoolVal (not b)
    |   (Negate, IntVal i) => IntVal (~i)
    |   _ => raise errorInEvalNExp
and evalifExp( e1:exp, e2:exp, e3:exp,env:environment):value = 
    case evalExp(e1, env) of 
        BoolVal true =>  evalExp(e2, env)
    |   BoolVal false => evalExp(e3, env)
    |   _ => raise errorInEvalIfExp
end