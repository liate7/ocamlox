open ContainersLabels
include Ast_type

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

let rec place_to_sexp = function
  | Variable_p id -> Sexp.atom @@ Id.to_string id
  | Field (expr, id) ->
      Sexp.(list [ atom "obj_ref"; expr_to_sexp expr; atom @@ Id.to_string id ])

and expr_to_sexp : (literal, place) expr -> Sexp.t = function
  | Binary (l, op, r) ->
      Sexp.(list [ atom @@ binop_to_string op; expr_to_sexp l; expr_to_sexp r ])
  | Unary (op, expr) ->
      Sexp.(list [ atom @@ unary_op_to_string op; expr_to_sexp expr ])
  | Grouping expr -> expr_to_sexp expr
  | Literal lit -> Sexp.atom @@ literal_to_string lit
  | Variable id -> place_to_sexp id
  | Assign (p, expr) ->
      Sexp.(list [ atom "set!"; place_to_sexp p; expr_to_sexp expr ])
  | Logic (l, Or, r) ->
      Sexp.(list [ atom "or"; expr_to_sexp l; expr_to_sexp r ])
  | Logic (l, And, r) ->
      Sexp.(list [ atom "and"; expr_to_sexp l; expr_to_sexp r ])
  | Call (f, args) ->
      Sexp.(
        list (atom "call" :: expr_to_sexp f :: List.map ~f:expr_to_sexp args))
  | Lambda f -> Sexp.(list (atom "λ" :: func f))
  | This -> Sexp.atom "this"

and func { params; body } =
  Sexp.
    [ list (List.map ~f:Fun.(Id.to_string %> atom) params); stmt_to_sexp body ]

and stmt_to_sexp = function
  | Expr e -> Sexp.(list [ atom "do"; expr_to_sexp e ])
  | Log e -> Sexp.(list (atom "log" :: List.map ~f:expr_to_sexp e))
  | If { condition; if_true; if_false } ->
      Sexp.(
        list
          (atom "if" :: expr_to_sexp condition :: stmt_to_sexp if_true
          :: List.map ~f:stmt_to_sexp (Option.to_list if_false)))
  | Block s -> Sexp.(list (atom "progn" :: List.map ~f:decl_to_sexp s))
  | While { condition; body } ->
      Sexp.(list [ atom "while"; expr_to_sexp condition; stmt_to_sexp body ])
  | Return s ->
      Sexp.(list (atom "return" :: Option.to_list (Option.map expr_to_sexp s)))

and decl_to_sexp = function
  | Var (id, e) ->
      Sexp.(list [ atom "let"; atom @@ Id.to_string id; expr_to_sexp e ])
  | Stmt s -> stmt_to_sexp s
  | Fun (id, f) -> Sexp.(list (atom "defn" :: atom (Id.to_string id) :: func f))
  | Class (id, methods) ->
      let method_to_sexp (id, f) =
        Sexp.(list (atom (Id.to_string id) :: func f))
      in
      Sexp.(
        list
          [
            atom "class";
            atom @@ Id.to_string id;
            list @@ List.map ~f:method_to_sexp methods;
          ])

let to_sexp = decl_to_sexp
let to_string t = to_sexp t |> Format.to_string Sexp.pp
