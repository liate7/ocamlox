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
type value = Number of float | String of string | True | False | Nil

type expr =
  | Binary of expr * binop * expr
  | Unary of unary_op * expr
  | Grouping of expr
  | Literal of value

type t = expr

let number_lit f = Literal (Number f)
let string_lit s = Literal (String s)

open ContainersLabels

let binop_to_string = function
  | Equal -> "="
  | Not_equal -> "!="
  | Less_than -> "<"
  | Less_equal -> "<="
  | Greater_than -> ">"
  | Greater_equal -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Comma -> "seq"

let unary_op_to_string = function Negate -> "negate" | Not -> "not"

let literal_to_string = function
  | Number n ->
      if Int.of_float n |> Float.of_int =. n then Int.(of_float n |> to_string)
      else Float.to_string n
  | String s -> "\"" ^ String.escaped s ^ "\""
  | True -> "true"
  | False -> "false"
  | Nil -> "nil"

let rec to_sexp t =
  match t with
  | Binary (l, op, r) ->
      Sexp.(list [ atom @@ binop_to_string op; to_sexp l; to_sexp r ])
  | Unary (op, expr) ->
      Sexp.(list [ atom @@ unary_op_to_string op; to_sexp expr ])
  | Grouping expr -> to_sexp expr
  | Literal lit -> Sexp.atom @@ literal_to_string lit

let to_string t = to_sexp t |> Format.to_string Sexp.pp
