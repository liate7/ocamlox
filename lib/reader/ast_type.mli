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

module Id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

type ('literal, 'place) expr =
  | Binary of ('literal, 'place) expr * binop * ('literal, 'place) expr
  | Unary of unary_op * ('literal, 'place) expr
  | Grouping of ('literal, 'place) expr
  | Assign of 'place * ('literal, 'place) expr
  | Logic of ('literal, 'place) expr * logic_op * ('literal, 'place) expr
  | Call of ('literal, 'place) expr * ('literal, 'place) expr list
  | Lambda of ('literal, 'place) func
  | This
  | Literal of 'literal
  | Variable of 'place

and ('literal, 'place) func = {
  params : Id.t list;
  body : ('literal, 'place) stmt;
}

and ('literal, 'place) stmt =
  | Expr of ('literal, 'place) expr
  | Log of ('literal, 'place) expr list
  | Block of ('literal, 'place) decl list
  | If of {
      condition : ('literal, 'place) expr;
      if_true : ('literal, 'place) stmt;
      if_false : ('literal, 'place) stmt option;
    }
  | While of {
      condition : ('literal, 'place) expr;
      body : ('literal, 'place) stmt;
    }
  | Return of ('literal, 'place) expr option

and ('literal, 'place) decl =
  | Var of Id.t * ('literal, 'place) expr
  | Fun of Id.t * ('literal, 'place) func
  | Stmt of ('literal, 'place) stmt
  | Class of Id.t * (Id.t * ('literal, 'place) func) list

type ('literal, 'place) t = ('literal, 'place) decl
type literal = Number of float | String of string | True | False | Nil
type place = Variable_p of Id.t | Field of (literal, place) expr * Id.t

val number_lit : float -> (literal, 'a) expr
val string_lit : string -> (literal, 'a) expr
