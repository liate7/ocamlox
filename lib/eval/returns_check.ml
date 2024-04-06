open! ContainersLabels
open Reader

(* type function_type = Top | Function *)

let rec decl_valid = function
  | Ast.Var (_, _) -> true
  | Ast.Fun (_, _, _) -> true
  | Ast.Stmt s -> stmt_valid s

and stmt_valid = function
  | Ast.Expr _ -> true
  | Ast.Log _ -> true
  | Ast.Block ds -> List.for_all ~f:decl_valid ds
  | Ast.If { condition = _; if_true; if_false } ->
      stmt_valid if_true && Option.map_or ~default:true stmt_valid if_false
  | Ast.While { condition = _; body } -> stmt_valid body
  | Ast.Return _ -> false

let go ast =
  if List.for_all ~f:decl_valid ast then Ok ast
  else Error.of_string "can't return outside function"
