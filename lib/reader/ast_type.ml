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

module Id = struct
  type t = String.t [@@deriving compare]

  let equal = String.equal
  let hash = String.hash
  let of_string str = str
  let to_string t = t
  let ( = ) = equal
end

type ('literal, 'place) expr =
  | Binary of ('literal, 'place) expr * binop * ('literal, 'place) expr
  | Unary of unary_op * ('literal, 'place) expr
  | Grouping of ('literal, 'place) expr
  | Assign of 'place * ('literal, 'place) expr
  | Logic of ('literal, 'place) expr * logic_op * ('literal, 'place) expr
  | Call of ('literal, 'place) expr * ('literal, 'place) expr list
  | Lambda of ('literal, 'place) func
  | Literal of 'literal
  | Get of 'place

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
  | Class of {
      name : Id.t;
      superclass : 'place option;
      methods : (Id.t * ('literal, 'place) func) list;
    }

type ('literal, 'place) t = ('literal, 'place) decl
type literal = Number of float | String of string | True | False | Nil

type place =
  | Variable of Id.t
  | Field of (literal, place) expr * Id.t
  | This
  | Super of Id.t

let number_lit f = Literal (Number f)
let string_lit s = Literal (String s)
