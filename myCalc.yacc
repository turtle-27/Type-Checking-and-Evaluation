(* User declarations *)
fun lookup "TRUE" = "TRUE, "
    | lookup "FALSE" = "FALSE, "

%%
(* required declarations*)
%name myCalc

%term 
    CONST of string | ID of string | NOT | AND | OR | XOR | EQUALS
|   IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | EOF | TERM

%nonterm program of string | temp of string | statement of string | formula of string
%pos int

(* optional declarations *)
%eop EOF
%noshift EOF

%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%start program 

%verbose

%%
program: temp statement (temp^statement^"program gives temp statement, ")
        | statement (statement^" program gives statement, ")

temp: temp statement (temp^statement^"temp gives temp statement, ")
    | statement (statement^" temp gives statement, ")
    
statement: formula TERM (formula^" TERM ;, "^"statement gives formula TERM, ")

formula: CONST ("CONST "^lookup CONST^"formula gives CONST, ")
    |   ID ("ID "^ID^", formula gives ID, ")
    |   NOT formula ("NOT NOT, "^formula^"formula gives NOT formula, ")
    |   formula AND formula (formula1^"AND AND, "^formula2^"formula gives formula AND formula, ")
    |   formula OR formula (formula1^"OR OR, "^formula2^"formula gives formula OR formula, ")
    |   formula XOR formula (formula1^"XOR XOR, "^formula2^"formula gives formula XOR formula, ")
    |   formula EQUALS formula (formula1^"EQUALS EQUALS, "^formula2^"formula gives formula EQUALS formula, ")
    |   formula IMPLIES formula (formula1^"IMPLIES IMPLIES, "^formula2^"formula gives formula IMPLIES formula, ")
    |   IF formula THEN formula ELSE formula ("IF IF, "^formula1^"THEN THEN, "^formula2^"ELSE ELSE, "^formula3^"formula gives IF formula THEN formula ELSE formula, ")
    |   LPAREN formula RPAREN ("LPAREN (, "^formula^"RPAREN ), formula gives LPAREN formula RPAREN, ")
