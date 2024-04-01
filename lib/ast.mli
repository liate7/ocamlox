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

type unary_op = Negate | Not
type value = Number of float | String of string | True | False | Nil

type expr =
  | Binary of expr * binop * expr
  | Unary of unary_op * expr
  | Grouping of expr
  | Literal of value

type t = expr

val number_lit : float -> expr
val string_lit : string -> expr
val to_string : t -> string
