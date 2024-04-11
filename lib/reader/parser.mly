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
%token LOG RETURN SUPER THIS TRUE LET WHILE LAMBDA

%token EOF

%nonassoc IF_NO_ELSE
%nonassoc ELSE

%{
    open! ContainersLabels
	open Ast_type
%}

%start <(Ast.literal, Ast.place) Ast_type.t list> program
%%

program:
  | s = declaration; rest = program { s :: rest }
  | EOF { [] };

declaration:
  | LET; id = id; "="; e = expression; ";"
    { Var (id, e) }
  | d = definition { d }
  | s = statement
    { Stmt s }
  | c = class_ { c };

definition:
  | DEFN; id = id; fn = func
    { Fun (id, fn) };

func:
  | "("; params = separated_list(",", id); ")"; body = statement
    { { params; body } };

class_:
  | CLASS; id = id; superclass = preceded("<", id)?;
    "{"; methods = methd*; "}"
    { Class {
		  name = id;
		  superclass = Option.map (fun i -> Variable i) superclass;
		  methods
		}
	};

methd:
  | id = id; f = func { id, f }

block:
  | "{"; s = declaration*; "}" { s }

statement:
  | e = expression; ";"      { Expr e }
  | LOG; e = expression; ";" { Log [e] }
  | s = if_statement         { s }
  | s = block                { Block s }
  | s = while_statement      { s }
  | s = return_statement     { s }

return_statement:
  | RETURN; e = expression; ";" { Return (Some e) }
  | RETURN; ";"                 { Return None }

while_statement:
  | WHILE; "("; cond = expression; ")"; body = statement
    { While { condition = cond; body } }

if_statement:
  | IF; "("; e = expression; ")"; t = statement; ELSE; f = statement
    { If { condition = e; if_true = t; if_false = Some f } }
  | IF; "("; e = expression; ")"; t = statement; %prec IF_NO_ELSE
    { If { condition = e; if_true = t; if_false = None } };

expression:
  /* | l = equality; ","; r = expression */
  /*   { Binary(l, Comma, r) } */
  | e = assignment { e };

assignment:
  | p = place; "="; e = assignment
    { Assign (p, e) }
  | e = logic_or { e };

place:
  | e = call; "."; id = id { Field (e, id) }
  | id = id                { Variable id };

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
  | e = call       { e };

call:
  | f = call; "("; args = separated_list(",", expression); ")"
    { Call (f, args) }
  | o = call; "."; id = id
    { Get (Field (o, id)) }
  | e = primary
    { e }

primary:
  | num = NUMBER             { number_lit num }
  | str = STRING             { string_lit str }
  | TRUE                     { Literal True }
  | FALSE                    { Literal False }
  | NIL                      { Literal Nil }
  | THIS                     { Get This }
  | SUPER "." attr = id      { Get (Super attr) }
  | id = id                  { Get (Variable id) }
  | lam = lambda             { lam }
  | "("; e = expression; ")" { Grouping e };

lambda:
  | LAMBDA; fn = func
    { Lambda fn }

id: id = IDENTIFIER { Id.of_string id }
