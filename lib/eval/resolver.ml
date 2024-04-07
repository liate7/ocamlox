open! ContainersLabels
open Reader

type place =
  | Var of (Id.t * int option)
  | Field of (Ast.literal, place) Ast.expr * Id.t

type t = (Ast.literal, place) Ast.t
type input = (Ast.literal, Ast.place) Ast.t

module Map = Map.Make (Id)
open Result.Infix

type function_type = Top | Function
type obj_type = Class | Not

let declare scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name false cur :: rest

let declaring scopes name =
  match scopes with
  | cur :: _ -> not @@ Map.get_or name cur ~default:true
  | [] -> false

let define scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name true cur :: rest

let transform_var scopes var : place =
  let rec loop acc = function
    | [] -> Var (var, None)
    | cur :: _ when Map.mem var cur -> Var (var, Some acc)
    | _ :: rest -> loop (acc + 1) rest
  in
  loop 0 scopes

let rec decl ftype otype scopes :
    input -> (bool Map.t list * t, [> Error.t ]) result = function
  | Ast.Var (id, init) ->
      let scopes = declare scopes id in
      let+ init = expr otype scopes init in
      (define scopes id, Ast.Var (id, init))
  | Ast.Fun (name, f) ->
      let scopes = define (declare scopes name) name in
      let+ f = handle_func otype scopes f in
      (scopes, Ast.Fun (name, f))
  | Ast.Stmt s ->
      let+ s = stmt ftype otype scopes s in
      (scopes, Ast.Stmt s)
  | Ast.Class (name, methods) ->
      let scopes = define (declare scopes name) name
      and names, funcs = List.split methods in
      let inner = Map.singleton (Id.of_string "this") true :: scopes in
      let inner =
        List.fold_left names ~init:inner ~f:(fun scopes name ->
            define (declare scopes name) name)
      in
      let+ funcs = Result.map_l (handle_func Class inner) funcs in
      (scopes, Ast.Class (name, List.combine names funcs))

and handle_func otype scopes { params; body } =
  let inner = List.fold_left ~init:(Map.empty :: scopes) ~f:define params in
  let+ body = stmt Function otype inner body in
  { Ast.params; body }

and stmt ftype otype scopes = function
  | Ast.Expr e ->
      let+ e = expr otype scopes e in
      Ast.Expr e
  | Ast.Log e ->
      let+ e = e |> Result.map_l (expr otype scopes) in
      Ast.Log e
  | Ast.Block decls ->
      let+ _, body =
        Result.fold_l
          (fun (scopes, acc) d ->
            let+ scopes, decl = decl ftype otype scopes d in
            (scopes, decl :: acc))
          (Map.empty :: scopes, [])
          decls
      in
      Ast.Block (List.rev body)
  | Ast.If { condition; if_true; if_false } -> (
      let* condition = expr otype scopes condition
      and* if_true = stmt ftype otype scopes if_true in
      match if_false with
      | None -> Ok (Ast.If { condition; if_true; if_false = None })
      | Some if_false ->
          let+ if_false = stmt ftype otype scopes if_false in
          Ast.If { condition; if_true; if_false = Some if_false })
  | Ast.While { condition; body } ->
      let+ condition = expr otype scopes condition
      and+ body = stmt ftype otype scopes body in
      Ast.While { condition; body }
  | Ast.Return e -> (
      match (ftype, e) with
      | Top, _ -> Error.of_string "can't return outside function"
      | _, None -> Ok (Ast.Return None)
      | _, Some e ->
          let+ e = expr otype scopes e in
          Ast.Return (Some e))

and expr otype scopes = function
  | Ast.Binary (l, op, r) ->
      let+ l = expr otype scopes l and+ r = expr otype scopes r in
      Ast.Binary (l, op, r)
  | Ast.Unary (op, e) ->
      let+ e = expr otype scopes e in
      Ast.Unary (op, e)
  | Ast.Grouping e ->
      let+ e = expr otype scopes e in
      Ast.Grouping e
  | Ast.Assign (Ast.Variable_p id, e) ->
      let+ e = expr otype scopes e in
      let var = transform_var scopes id in
      Ast.Assign (var, e)
  | Ast.Assign (Ast.Field (obj, attr), e) ->
      let+ obj = expr otype scopes obj and+ e = expr otype scopes e in
      Ast.Assign (Field (obj, attr), e)
  | Ast.Logic (l, op, r) ->
      let+ l = expr otype scopes l and+ r = expr otype scopes r in
      Ast.Logic (l, op, r)
  | Ast.Call (f, args) ->
      let+ f = expr otype scopes f
      and+ args = Result.map_l (expr otype scopes) args in
      Ast.Call (f, args)
  | Ast.Lambda f ->
      let+ f = handle_func otype scopes f in
      Ast.Lambda f
  | Ast.Literal lit -> Ok (Ast.Literal lit)
  | Ast.Variable (Ast.Variable_p var) ->
      if declaring scopes var then
        Error.of_string
          [%string
            {|can't reference "%{Id.to_string var}" while shadowing it.|}]
      else Ok (Ast.Variable (transform_var scopes var))
  | Ast.Variable (Ast.Field (e, attr)) ->
      let+ e = expr otype scopes e in
      Ast.Variable (Field (e, attr))
  | Ast.This -> (
      match otype with
      | Class -> Ok (Ast.Variable (transform_var scopes @@ Id.of_string "this"))
      | Not -> Error.of_string {|can't reference "this" outside a class|})

let go ast =
  let+ scopes, ast =
    Result.fold_l
      (fun (scopes, acc) d ->
        let+ scopes, d = decl Top Not scopes d in
        (scopes, d :: acc))
      ([], []) ast
  in
  assert (List.is_empty scopes);
  List.rev ast
