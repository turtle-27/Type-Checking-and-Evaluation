structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val out = ref ("")
  val eof = fn () => (print("["^(!out)^"]\n"); Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor myCalcLexFun(structure Tokens:myCalc_TOKENS));

alpha=[A-Za-z];
ws = [\ \t\n];
%%
\n       => (lex());
{ws}+    => (lex());

";"      => (out := (!out)^"TERM "^"\""^";"^"\""; pos := (!pos) + 1;  Tokens.TERM(!pos, !pos));
"_"      => (out := (!out)^"TERM "^"\""^" "^"\""; pos := (!pos) + 1;  Tokens.TERM(!pos, !pos));
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
"IF"   => (out := (!out)^"IF "^"\""^"IF"^"\""^", "; Tokens.IF(!pos, !pos));
"THEN"   => (out := (!out)^"THEN "^"\""^"THEN"^"\""^", " ; Tokens.THEN(!pos, !pos));
"ELSE"   => (out := (!out)^"ELSE "^"\""^"ELSE"^"\""^", "; Tokens.ELSE(!pos, !pos));
{alpha}+ => (out := (!out)^"ID "^"\""^yytext^"\""^", " ; Tokens.ID(yytext,!pos,!pos));
.      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
