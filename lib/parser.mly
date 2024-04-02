%token LEFT_PAREN "(" RIGHT_PAREN ")"
%token LEFT_BRACE "{" RIGHT_BRACE "}"
%token COMMA "," DOT "." SEMICOLON ";"
%token PLUS "+" MINUS "-" SLASH "/" STAR "*"

%token BANG "!" BANG_EQUAL "!="
%token EQUAL "=" EQUAL_EQUAL "=="
%token GREATER ">" GREATER_EQUAL ">="
%token LESS "<" LESS_EQUAL "<="

%token <string> IDENTIFIER
%token <string> STRING
%token <float> NUMBER

%token AND CLASS ELSE FALSE DEFN FOR IF NIL OR 
%token LOG RETURN SUPER THIS TRUE LET WHILE

%token EOF

%{
	open Ast_type
%}

%start <Ast_type.t option> prog
%%

prog:
  | e = expression; EOF { Some e }
  | EOF { None };

expression:
  | l = equality; ","; r = expression
    { Binary(l, Comma, r) }
  | e = equality { e };

equality:
  | l = equality; "!="; r = comparison
    { Binary(l, Not_equal, r) }
  | l = equality; "=="; r = comparison
    { Binary(l, Equal, r) }
  | e = comparison { e };

comparison:
  | l = comparison; op = compare_op; r = term
    { Binary(l, op, r) }
  | e = term { e };

compare_op:
  | LESS          { Less_than }
  | LESS_EQUAL    { Less_equal }
  | GREATER       { Greater_than }
  | GREATER_EQUAL { Greater_equal }

term:
  | l = term; "+"; r = factor
    { Binary(l, Plus, r) }
  | l = term; "-"; r = factor
    { Binary(l, Minus, r) }
  | e = factor { e };

factor:
  | l = factor; "*"; r = unary
	{ Binary(l, Times, r) }
  | l = factor; "/"; r = unary
	{ Binary(l, Div, r) }
  | e = unary
    { e };

unary:
  | "!"; e = unary { Unary(Not, e) }
  | "-"; e = unary { Unary(Negate, e) }
  | e = primary         { e };

primary:
  | num = NUMBER             { number_lit num }
  | str = STRING             { string_lit str }
  | TRUE                     { Literal True }
  | FALSE                    { Literal False }
  | NIL                      { Literal Nil }
  | "("; e = expression; ")" { Grouping e };
