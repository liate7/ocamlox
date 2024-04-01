%token LEFT_PAREN "(" RIGHT_PAREN ")"
%token LEFT_BRACE "{" RIGHT_BRACE "}"
%token COMMA "," DOT "." SEMICOLON ";"
%token PLUS "+" MINUS "-" SLASH "/" STAR "*"

%token BANG BANG_EQUAL
%token EQUAL EQUAL_EQUAL
%token GREATER GREATER_EQUAL
%token LESS LESS_EQUAL

%token <string> IDENTIFIER
%token <string> STRING
%token <float> NUMBER

%token AND CLASS ELSE FALSE DEFN FOR IF NIL OR 
%token LOG RETURN SUPER THIS TRUE LET WHILE

%token EOF

%start <Ast.t option> prog
%%

prog:
  | EOF { None };
