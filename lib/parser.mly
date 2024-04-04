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

%nonassoc IF_NO_ELSE
%nonassoc ELSE

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
  | LOG; e = expression; ";"   { Log [e] }
  | s = if_statement           { s }
  | "{"; s = declaration*; "}" { Block s }
  | s = while_statement        { s }

while_statement:
  | WHILE; "("; cond = expression; ")"; body = statement
    { While { condition = cond; body } }

if_statement:
  | IF; "("; e = expression; ")"; t = statement; ELSE; f = statement
    { If { condition = e; if_true = t; if_false = Some f } }
  | IF; "("; e = expression; ")"; t = statement; %prec IF_NO_ELSE
    { If { condition = e; if_true = t; if_false = None } };

expression:
  | l = equality; ","; r = expression
    { Binary(l, Comma, r) }
  | e = assignment { e };

assignment:
  | p = place; "="; e = assignment
    { Assign (p, e) }
  | e = logic_or { e };

place: id = IDENTIFIER { Variable_p (Id.of_string id) };

logic_or:
  | l = logic_or; OR; r = logic_and
    { Logic(l, Or, r) }
  | e = logic_and { e };

logic_and:
  | l = logic_and; AND; r = equality
    { Logic(l, And, r) }
  | e = equality 
    { e };

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
