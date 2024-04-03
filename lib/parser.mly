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

%start <Ast_type.t list> program
%%

program:
  | s = declaration; rest = program { s :: rest }
  | EOF { [] };

declaration:
  | LET; id = IDENTIFIER; "="; e = expression; ";"
    { Var (Id.of_string id, e) }
  | s = statement
    { Stmt s };

statement:
  | e = expression; ";"        { Expr e }
  | LOG; e = expression*; ";"  { Log e }
  | "{"; s = declaration*; "}" { Block s };

expression:
  | l = equality; ","; r = expression
    { Binary(l, Comma, r) }
  | e = assignment { e };

assignment:
  | p = place; "="; e = assignment
    { Assign (p, e) }
  | e = equality { e };

place: id = IDENTIFIER { Variable_p (Id.of_string id) }

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
  | e = primary    { e };

primary:
  | num = NUMBER             { number_lit num }
  | str = STRING             { string_lit str }
  | TRUE                     { Literal True }
  | FALSE                    { Literal False }
  | NIL                      { Literal Nil }
  | id = IDENTIFIER          { Variable (Id.of_string id) }
  | "("; e = expression; ")" { Grouping e };
