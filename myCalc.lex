structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val out = ref ("")
  val eof = fn () => (print("["^(!out)^"]\n");  out := "" ; Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  

%%
%header (functor myCalcLexFun(structure Tokens:myCalc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t\n];
%%
\n       => (lex());
{ws}+    => (lex());
";"      => (out := (!out)^"TERM "^"\""^";"^"\""; pos := (!pos) + 1;  Tokens.TERM(!pos, !pos));
"let"   => (out := (!out)^"LET "^"\""^"LET"^"\""^", "; Tokens.LET(!pos, !pos));
"in"   => (out := (!out)^"IN "^"\""^"IN"^"\""^", "; Tokens.IN(!pos, !pos));
"end"   => (out := (!out)^"END "^"\""^"END"^"\""^", "; Tokens.END(!pos, !pos));
"TRUE"   => (out := (!out)^"CONST "^"\""^"TRUE"^"\""^", "; Tokens.CONST(yytext, !pos, !pos));
"FALSE"  => (out := (!out)^"CONST "^"\""^"FALSE"^"\""^", "; Tokens.CONST(yytext, !pos, !pos));
"NOT"      => (out := (!out)^"NOT "^"\""^"NOT"^"\""^", "; Tokens.NOT(!pos,!pos));
"AND"      => (out := (!out)^"AND "^"\""^"AND"^"\""^", "; Tokens.AND(!pos,!pos));
"("      => (out := (!out)^"LPAREN "^"\""^"("^"\""^", "; Tokens.LPAREN(!pos,!pos));
")"      => (out := (!out)^"RPAREN "^"\""^")"^"\""^", "; Tokens.RPAREN(!pos,!pos));
"OR"      => (out := (!out)^"OR "^"\""^"OR"^"\""^", "; Tokens.OR(!pos,!pos));
"XOR"      => (out := (!out)^"XOR "^"\""^"XOR"^"\""^", "; Tokens.XOR(!pos,!pos));
"EQUALS"   => (out := (!out)^"EQUALS "^"\""^"EQUALS"^"\""^", "; Tokens.EQUALS(!pos, !pos));
"IMPLIES"   => (out := (!out)^"IMPLIES "^"\""^"IMPLIES"^"\""^", "; Tokens.IMPLIES(!pos, !pos));
"NEGATE"   =>  (out := (!out)^"NEGATE "^"\""^"NEGATE"^"\""^", "; Tokens.NEGATE(!pos, !pos));
"if"   =>  (out := (!out)^"IF "^"\""^"IF"^"\""^", "; Tokens.IF(!pos, !pos));
"then"   => (out := (!out)^"THEN "^"\""^"THEN"^"\""^", " ; Tokens.THEN(!pos, !pos));
"else"   => (out := (!out)^"ELSE "^"\""^"ELSE"^"\""^", "; Tokens.ELSE(!pos, !pos));
"fi"    =>  (out := (!out)^"FI "^"\""^"FI"^"\""^", "; Tokens.FI(!pos, !pos));
"LESSTHAN"  =>  (out := (!out)^"LESSTHAN "^"\""^"LESSTHAN"^"\""^", "; Tokens.LESSTHAN(!pos, !pos));
"GREATERTHAN" =>  (out := (!out)^"GREATERTHAN "^"\""^"GREATERTHAN"^"\""^", "; Tokens.GREATERTHAN(!pos, !pos));
"PLUS"      => (out := (!out)^"PLUS "^"\""^"PLUS"^"\""^", "; Tokens.PLUS(!pos,!pos));
"TIMES"      => (out := (!out)^"TIMES "^"\""^"TIMES"^"\""^", "; Tokens.TIMES(!pos,!pos));
"MINUS"      => (out := (!out)^"MINUS "^"\""^"MINUS"^"\""^", "; Tokens.MINUS(!pos,!pos));
"="      => (out := (!out)^"EQ "^"\""^"="^"\""^", "; Tokens.EQ(!pos,!pos)); 
{digit}+ => (out := (!out)^"NUM "^"\""^yytext^"\""^", "; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
{alpha}+ => (out := (!out)^"ID "^"\""^yytext^"\""^", " ; Tokens.ID(yytext,!pos,!pos));
.      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
