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
type literal = Number of float | String of string | True | False | Nil

type expr =
  | Binary of expr * binop * expr
  | Unary of unary_op * expr
  | Grouping of expr
  | Literal of literal

type t = expr

let number_lit f = Literal (Number f)
let string_lit s = Literal (String s)
