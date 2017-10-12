(* Required for parser *)
structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val pos = ref 0
val error = fn (e, l: int, _) =>
              print ("(line " ^ (Int.toString l) ^ ": " ^ e ^ ") reporterror\n")

val charlist: char list ref = ref []

fun addChar s =
  charlist := (String.sub (s, 0))::(!charlist)

val firstComment : string option ref = ref NONE

structure IM = RedBlackMapFn (struct
	    type ord_key = int
	    val compare = Int.compare
	    end)

val file = ref 0
val lines : (int * int) list ref = ref []
val files : string IM.map ref = ref (IM.insert (IM.empty, 0 , "<top>"))
val nextFile = ref 1

fun getFileName n =
    getOpt (IM.find (!files, n), "")


val eof = fn () => Tokens.EOF (!pos, !file)

fun init () = (
  pos := 1;
  lines := [];
  files := (IM.insert (IM.empty, 0 , "<top>"));
  nextFile := 1;
  file := 0;
  firstComment := NONE
  )

%%
%header (functor EQNLexFun (structure Tokens : EQN_TOKENS));

%s STRING;
%full

NL = (\n | \013\n | \013);
WS = (" " | \t);
EXPONENT = (("E" | "e") "-"? "0"* [1-9][0-9]*);
REAL = ([0-9][0-9]*("." [0-9]*)?{EXPONENT}?) | ("." [0-9][0-9]* {EXPONENT}?);
SUPTWO = \194\178;
SUPTHREE = \194\179;
PI = \207\128;
VAR = [a-z];
%%

<INITIAL> {NL} => (pos := !pos + 1; lex ());
<INITIAL> {WS} => (lex ());
<INITIAL> "(" => (Tokens.LPAREN (!pos, !file));
<INITIAL> "\[" => (Tokens.LBRACK (!pos, !file));
<INITIAL> "\{" => (Tokens.LBRACE (!pos, !file));
<INITIAL> ")" => (Tokens.RPAREN (!pos, !file));
<INITIAL> "\]" => (Tokens.RBRACK (!pos, !file));
<INITIAL> "\}" => (Tokens.RBRACE (!pos, !file));
<INITIAL> "|" => (Tokens.BAR (!pos, !file));
<INITIAL> "+" => (Tokens.PLUS (!pos, !file));
<INITIAL> "-" => (Tokens.MINUS (!pos, !file));
<INITIAL> "*" => (Tokens.TIMES (!pos, !file));
<INITIAL> "\^" => (Tokens.POW (!pos, !file));
<INITIAL> "/" => (Tokens.DIV (!pos, !file));
<INITIAL> "mod" => (Tokens.MOD (!pos, !file));
<INITIAL> "=" => (Tokens.EQUALS (!pos, !file));
<INITIAL> "<" => (Tokens.LT (!pos, !file));
<INITIAL> ">" => (Tokens.GT (!pos, !file));
<INITIAL> "<=" => (Tokens.LE (!pos, !file));
<INITIAL> ">=" => (Tokens.GE (!pos, !file));
<INITIAL> "&&" => (Tokens.AND (!pos, !file));
<INITIAL> "&" => (Tokens.AND (!pos, !file));
<INITIAL> "||" => (Tokens.OR (!pos, !file));
<INITIAL> "|" => (Tokens.OR (!pos, !file));
<INITIAL> "," => (Tokens.COMMA (!pos, !file));
<INITIAL> "x" => (Tokens.X (!pos, !file));
<INITIAL> "X" => (Tokens.X (!pos, !file));
<INITIAL> "y" => (Tokens.Y (!pos, !file));
<INITIAL> "Y" => (Tokens.Y (!pos, !file));
<INITIAL> "r" => (Tokens.R (!pos, !file));
<INITIAL> "th" => (Tokens.THETA (!pos, !file));
<INITIAL> "pi" => (Tokens.PI (!pos, !file));
<INITIAL> "e" => (Tokens.E (!pos, !file));
<INITIAL> "sqrt" => (Tokens.SQRT (!pos, !file));
<INITIAL> "floor" => (Tokens.FLOOR (!pos, !file));
<INITIAL> "cos" => (Tokens.COS (!pos, !file));
<INITIAL> "sin" => (Tokens.SIN (!pos, !file));
<INITIAL> "tan" => (Tokens.TAN (!pos, !file));
<INITIAL> "erf" => (Tokens.ERF (!pos, !file));
<INITIAL> "ln" => (Tokens.LN (!pos, !file));
<INITIAL> "log" => (Tokens.LN (!pos, !file));
<INITIAL> "lg" => (Tokens.LG (!pos, !file));
<INITIAL> "acos" => (Tokens.ACOS (!pos, !file));
<INITIAL> "arccos" => (Tokens.ACOS (!pos, !file));
<INITIAL> "asin" => (Tokens.ASIN (!pos, !file));
<INITIAL> "arcsin" => (Tokens.ASIN (!pos, !file));
<INITIAL> "atan" => (Tokens.ATAN (!pos, !file));
<INITIAL> "arctan" => (Tokens.ATAN (!pos, !file));
<INITIAL> "atan2" => (Tokens.ATAN2 (!pos, !file));
<INITIAL> "cosh" => (Tokens.COSH (!pos, !file));
<INITIAL> "sinh" => (Tokens.SINH (!pos, !file));
<INITIAL> "sinc" => (Tokens.SINC (!pos, !file));
<INITIAL> "tanh" => (Tokens.TANH (!pos, !file));
<INITIAL> "exp" =>  (Tokens.EXP (!pos, !file));
<INITIAL> "where" =>  (Tokens.WHERE (!pos, !file));
<INITIAL> "if" =>  (Tokens.IF (!pos, !file));
<INITIAL> "then" =>  (Tokens.THEN (!pos, !file));
<INITIAL> "else" =>  (Tokens.ELSE (!pos, !file));
<INITIAL> {SUPTWO} => (Tokens.SUPTWO (!pos, !file));
<INITIAL> {SUPTHREE} => (Tokens.SUPTHREE (!pos, !file));
<INITIAL> {PI} => (Tokens.PI (!pos, !file));
<INITIAL> {REAL} => (Tokens.REAL (valOf (Real.fromString yytext), !pos, !file));
<INITIAL> {VAR} => (Tokens.VAR(yytext, !pos, !file));
. => (error ("ignoring bad character "^yytext, !pos, !file); lex());
