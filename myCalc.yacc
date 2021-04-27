(* User declarations *)
fun checkBool "TRUE" = true
    | checkBool "FALSE" = false

%%
(* required declarations*)
%name myCalc

%term 
    CONST of string | ID of string | NOT | AND | OR | XOR | EQUALS | LET | IN | END | LESSTHAN | GREATERTHAN
|   IMPLIES | IF | THEN | ELSE | FI | NEGATE | LPAREN | RPAREN | EOF | TERM | MINUS | PLUS | TIMES | EQ | NUM of int 

%nonterm program of AST.exp list | temp of AST.exp list | statement of AST.exp | formula of AST.exp | decl of AST.decl
%pos int

(* optional declarations *)
%eop EOF
%noshift EOF

%left EQ
%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS
%left LESSTHAN GREATERTHAN
%left MINUS PLUS
%left TIMES
%right NOT NEGATE


%start program 

%verbose

%%
program: temp (temp)

temp: temp TERM statement (statement::temp)
    | statement (statement::[])


statement: formula (formula) 

decl: ID EQ formula (AST.ValDecl(ID, formula))

formula: CONST (AST.BoolExp(checkBool(CONST)))
    |   ID (AST.VarExp(ID))
    |   NUM (AST.NumExp(NUM))
    |   NOT formula (AST.NExp(AST.Not, formula))
    |   NEGATE formula  (AST.NExp(AST.Negate, formula))
    |   formula AND formula (AST.BinExp(AST.And, formula1, formula2))
    |   formula OR formula (AST.BinExp(AST.Or, formula1, formula2))
    |   formula XOR formula (AST.BinExp(AST.Xor, formula1, formula2))
    |   formula EQUALS formula (AST.BinExp(AST.Equals, formula1, formula2))
    |   formula IMPLIES formula (AST.BinExp(AST.Implies, formula1, formula2))
    |   IF formula THEN formula ELSE formula FI (AST.ifExp(formula1, formula2, formula3))
    |   LPAREN formula RPAREN (formula)
    |   formula LESSTHAN formula (AST.BinExp(AST.Lessthan, formula1, formula2))
    |   formula GREATERTHAN formula (AST.BinExp(AST.Greaterthan, formula1, formula2))
    |   formula PLUS formula (AST.BinExp(AST.Add, formula1, formula2))
    |   formula MINUS formula (AST.BinExp(AST.Sub, formula1, formula2))
    |   formula TIMES formula (AST.BinExp(AST.Mul, formula1, formula2))
    |   LET decl IN formula END (AST.LetExp(decl, formula))