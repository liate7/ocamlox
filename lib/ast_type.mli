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

module Id : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val ( = ) : t -> t -> bool
end

type place = Variable_p of Id.t

type expr =
  | Binary of expr * binop * expr
  | Unary of unary_op * expr
  | Grouping of expr
  | Literal of literal
  | Variable of Id.t
  | Assign of place * expr

type stmt = Expr of expr | Log of expr list | Block of decl list
and decl = Var of Id.t * expr | Stmt of stmt

type t = decl

val number_lit : float -> expr
val string_lit : string -> expr
