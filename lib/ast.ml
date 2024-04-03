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

let place_to_sexp = function Variable_p id -> Sexp.atom @@ Id.to_string id

let rec expr_to_sexp = function
  | Binary (l, op, r) ->
      Sexp.(list [ atom @@ binop_to_string op; expr_to_sexp l; expr_to_sexp r ])
  | Unary (op, expr) ->
      Sexp.(list [ atom @@ unary_op_to_string op; expr_to_sexp expr ])
  | Grouping expr -> expr_to_sexp expr
  | Literal lit -> Sexp.atom @@ literal_to_string lit
  | Variable id -> Sexp.atom @@ Id.to_string id
  | Assign (p, expr) ->
      Sexp.(list [ atom "set!"; place_to_sexp p; expr_to_sexp expr ])

let rec stmt_to_sexp = function
  | Expr e -> Sexp.(list [ atom "do"; expr_to_sexp e ])
  | Log e -> Sexp.(list (atom "log" :: List.map ~f:expr_to_sexp e))
  | Block s -> Sexp.(list (atom "progn" :: List.map ~f:decl_to_sexp s))

and decl_to_sexp = function
  | Var (id, e) ->
      Sexp.(list [ atom "let"; atom @@ Id.to_string id; expr_to_sexp e ])
  | Stmt s -> stmt_to_sexp s

let to_sexp = decl_to_sexp
let to_string t = to_sexp t |> Format.to_string Sexp.pp

module I = Parser.MenhirInterpreter

type error = [ `Syntax of string | `Parsing of string ]

let lexing_pos_to_string ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  let line = Int.to_string pos_lnum
  and col = Int.to_string (pos_cnum - pos_bol) in
  [%string "line %{line}, column %{col}"]

(* TODO: handle incomplete statements, as from repl inputs *)
(* I /think/ this looks like an extra error case with a checkpoint (or maybe a continuation?)
   value, but piping that through looks annoyingly complicated. *)
let of_lexbuf lexbuf =
  let rec loop chkpoint =
    match chkpoint with
    | I.InputNeeded _ ->
        let token = Lexer.read lexbuf
        and startp, endp = Sedlexing.lexing_positions lexbuf in
        loop @@ I.offer chkpoint (token, startp, endp)
    | I.Shifting _ | I.AboutToReduce _ -> loop @@ I.resume chkpoint
    | I.HandlingError _ ->
        let start, _ = Sedlexing.lexing_positions lexbuf
        and lexeme = Sedlexing.Utf8.lexeme lexbuf in
        Error
          (`Syntax
            [%string
              "Syntax error at %{lexing_pos_to_string start} (%{lexeme})"])
    | I.Accepted ast -> Ok ast
    | I.Rejected ->
        (* should only happen after a [I.HandlingError] event,
           which we never continue from *)
        assert false
  in
  let startp, _ = Sedlexing.lexing_positions lexbuf in
  try loop @@ Parser.Incremental.program startp
  with Lexer.SyntaxError msg -> Error (`Parsing msg)
