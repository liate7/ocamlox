type binop =
  | Equal
  | Not_equal
  | Less_than
  | Less_equal
  | Greater_than
  | Greater_equal
  | Plus
  | Minus
  | Times
  | Div
  | Comma

type unary_op = Negate | Not
type logic_op = And | Or
type 'var place = Variable_p of 'var

type ('literal, 'var) expr =
  | Binary of ('literal, 'var) expr * binop * ('literal, 'var) expr
  | Unary of unary_op * ('literal, 'var) expr
  | Grouping of ('literal, 'var) expr
  | Assign of 'var place * ('literal, 'var) expr
  | Logic of ('literal, 'var) expr * logic_op * ('literal, 'var) expr
  | Call of ('literal, 'var) expr * ('literal, 'var) expr list
  | Lambda of 'var list * ('literal, 'var) stmt
  | Literal of 'literal
  | Variable of 'var

and ('literal, 'var) stmt =
  | Expr of ('literal, 'var) expr
  | Log of ('literal, 'var) expr list
  | Block of ('literal, 'var) decl list
  | If of {
      condition : ('literal, 'var) expr;
      if_true : ('literal, 'var) stmt;
      if_false : ('literal, 'var) stmt option;
    }
  | While of { condition : ('literal, 'var) expr; body : ('literal, 'var) stmt }
  | Return of ('literal, 'var) expr option

and ('literal, 'var) decl =
  | Var of 'var * ('literal, 'var) expr
  | Fun of 'var * 'var list * ('literal, 'var) stmt
  | Stmt of ('literal, 'var) stmt

type ('literal, 'var) t = ('literal, 'var) decl
type literal = Number of float | String of string | True | False | Nil

module Id = struct
  type t = String.t [@@deriving compare]

  let equal = String.equal
  let hash = String.hash
  let of_string str = str
  let to_string t = t
  let ( = ) = equal
end

let number_lit f = Literal (Number f)
let string_lit s = Literal (String s)
