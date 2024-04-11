open ContainersLabels
open Reader
open Result.Infix

type input = (Ast.literal, Ast.place) Ast.t
type t = (Ast.literal, Ast.place) Ast.t

let init = Id.of_string "init"

let append_body body stmt =
  match body with
  | Ast.Block decls -> Ast.Block (List.append decls [ Ast.Stmt stmt ])
  | body -> Ast.(Block [ Stmt body; Stmt stmt ])

let rec decl_search in_init (d : (Ast.literal, Ast.place) Ast.decl) =
  match d with
  | Ast.Var (id, e) ->
      let+ e = expr_search e in
      Ast.Var (id, e)
  | Ast.Stmt s ->
      let+ s = stmt_search in_init s in
      Ast.Stmt s
  | Ast.Fun (id, { params; body }) ->
      let+ body = stmt_search false body in
      Ast.Fun (id, { Ast.params; body })
  | Ast.Class { name; superclass; methods } ->
      let+ methods = Result.map_l method_search methods in
      Ast.Class { name; superclass; methods }

and method_search (id, { params; body }) =
  let in_init = Id.(init = id) in
  let+ body = stmt_search in_init body in
  let body =
    if in_init then append_body body Ast.(Return (Some (Get This))) else body
  in
  (id, { Ast.params; body })

and stmt_search in_init s =
  match s with
  | Ast.Expr e ->
      let+ e = expr_search e in
      Ast.Expr e
  | Ast.Log es ->
      let+ es = Result.map_l expr_search es in
      Ast.Log es
  | Ast.Block ds ->
      let+ ds = Result.map_l (decl_search in_init) ds in
      Ast.Block ds
  | Ast.If { condition; if_true; if_false } -> (
      let* condition = expr_search condition
      and* if_true = stmt_search in_init if_true in
      match if_false with
      | None -> Ok (Ast.If { condition; if_true; if_false })
      | Some if_false ->
          let+ if_false = stmt_search in_init if_false in
          Ast.If { condition; if_true; if_false = Some if_false })
  | Ast.While { condition; body } ->
      let+ condition = expr_search condition
      and+ body = stmt_search in_init body in
      Ast.While { condition; body }
  | Ast.Return e -> (
      match (in_init, e) with
      | true, None -> Ok (Ast.Return (Some Ast.(Get This)))
      | true, Some _ ->
          Error.of_string "can't return a value from an initializer"
      | false, None -> Ok (Ast.Return None)
      | false, Some e ->
          let+ e = expr_search e in
          Ast.Return (Some e))

and expr_search e =
  match e with
  | Ast.Binary (l, op, r) ->
      let+ l = expr_search l and+ r = expr_search r in
      Ast.Binary (l, op, r)
  | Ast.Unary (op, e) ->
      let+ e = expr_search e in
      Ast.Unary (op, e)
  | Ast.Grouping e -> expr_search e
  | Ast.Assign (Ast.Variable id, e) ->
      let+ e = expr_search e in
      Ast.Assign (Ast.Variable id, e)
  | Ast.Assign (Ast.Field (obj, id), e) ->
      let+ obj = expr_search obj and+ e = expr_search e in
      Ast.Assign (Ast.Field (obj, id), e)
  | Ast.Assign (((Ast.Super _ | Ast.This) as place), e) ->
      let+ e = expr_search e in
      Ast.Assign (place, e)
  | Ast.Logic (l, op, r) ->
      let+ l = expr_search l and+ r = expr_search r in
      Ast.Logic (l, op, r)
  | Ast.Call (e, args) ->
      let+ e = expr_search e and+ args = Result.map_l expr_search args in
      Ast.Call (e, args)
  | Ast.Lambda { params; body } ->
      let+ body = stmt_search false body in
      Ast.Lambda { Ast.params; body }
  | Ast.Literal _ | Ast.Get (Ast.Variable _ | Ast.Super _ | Ast.This) -> Ok e
  | Ast.Get (Ast.Field (e, id)) ->
      let+ e = expr_search e in
      Ast.(Get (Field (e, id)))

let go ast = Result.map_l (decl_search false) ast
