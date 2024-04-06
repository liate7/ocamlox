open! ContainersLabels
open Reader

type id = Id.t * int option
type t = (Ast.literal, id) Ast.t
type input = (Ast.literal, Id.t) Ast.t

module Map = Map.Make (Id)
open Result.Infix

let declare scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name false cur :: rest

let declaring scopes name =
  match scopes with
  | cur :: _ -> not @@ Map.get_or name cur ~default:true
  | [] -> false

let define scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name true cur :: rest

let transform_id scopes id : id =
  let rec loop acc = function
    | [] -> (id, None)
    | cur :: _ when Map.mem id cur -> (id, Some acc)
    | _ :: rest -> loop (acc + 1) rest
  in
  loop 0 scopes

let rec decl scopes : input -> (bool Map.t list * t, [> Error.t ]) result =
  function
  | Ast.Var (id, init) ->
      let scopes = declare scopes id in
      let+ init = expr scopes init in
      (define scopes id, Ast.Var ((id, None), init))
  | Ast.Fun (name, params, body) ->
      let scopes = define (declare scopes name) name in
      let+ params, body = handle_func scopes params body in
      (scopes, Ast.Fun ((name, None), params, body))
  | Ast.Stmt s ->
      let+ s = stmt scopes s in
      (scopes, Ast.Stmt s)

and handle_func scopes params body =
  let inner = List.fold_left ~init:(Map.empty :: scopes) ~f:define params in
  let params = List.map ~f:(fun id -> (id, None)) params in
  let+ body = stmt inner body in
  (params, body)

and stmt scopes = function
  | Ast.Expr e ->
      let+ e = expr scopes e in
      Ast.Expr e
  | Ast.Log e ->
      let+ e = e |> Result.map_l (expr scopes) in
      Ast.Log e
  | Ast.Block decls ->
      let+ _, body =
        Result.fold_l
          (fun (scopes, acc) d ->
            let+ scopes, decl = decl scopes d in
            (scopes, decl :: acc))
          (Map.empty :: scopes, [])
          decls
      in
      Ast.Block (List.rev body)
  | Ast.If { condition; if_true; if_false } -> (
      let* condition = expr scopes condition in
      let* if_true = stmt scopes if_true in
      match if_false with
      | None -> Ok (Ast.If { condition; if_true; if_false = None })
      | Some if_false ->
          let+ if_false = stmt scopes if_false in
          Ast.If { condition; if_true; if_false = Some if_false })
  | Ast.While { condition; body } ->
      let* condition = expr scopes condition in
      let+ body = stmt scopes body in
      Ast.While { condition; body }
  | Ast.Return e -> (
      match e with
      | None -> Ok (Ast.Return None)
      | Some e ->
          let+ e = expr scopes e in
          Ast.Return (Some e))

and expr scopes = function
  | Ast.Binary (l, op, r) ->
      let* l = expr scopes l in
      let+ r = expr scopes r in
      Ast.Binary (l, op, r)
  | Ast.Unary (op, e) ->
      let+ e = expr scopes e in
      Ast.Unary (op, e)
  | Ast.Grouping e ->
      let+ e = expr scopes e in
      Ast.Grouping e
  | Ast.Assign (Ast.Variable_p id, e) ->
      let+ e = expr scopes e in
      let id = transform_id scopes id in
      Ast.Assign (Ast.Variable_p id, e)
  | Ast.Logic (l, op, r) ->
      let* l = expr scopes l in
      let+ r = expr scopes r in
      Ast.Logic (l, op, r)
  | Ast.Call (f, args) ->
      let* f = expr scopes f in
      let+ args = Result.map_l (expr scopes) args in
      Ast.Call (f, args)
  | Ast.Lambda (params, body) ->
      let+ params, body = handle_func scopes params body in
      Ast.Lambda (params, body)
  | Ast.Literal lit -> Ok (Ast.Literal lit)
  | Ast.Variable id ->
      if declaring scopes id then
        Error.of_string
          [%string {|can't reference "%{Id.to_string id}" while shadowing it.|}]
      else Ok (Ast.Variable (transform_id scopes id))

let go ast =
  let+ scopes, ast =
    Result.fold_l
      (fun (scopes, acc) d ->
        let+ scopes, d = decl scopes d in
        (scopes, d :: acc))
      ([], []) ast
  in
  assert (List.is_empty scopes);
  List.rev ast
