open! ContainersLabels
open! Eio.Std

type error = [ `Error of string ]

let error msg = Error (`Error msg)

module Obj = struct
  type t = Nil | Bool of bool | Number of float | String of string

  let to_string ?(readable = true) = function
    | Nil -> "nil"
    | Bool true -> "true"
    | Bool false -> "false"
    | Number n ->
        if Int.of_float n |> Float.of_int =. n then
          Int.(of_float n |> to_string)
        else Float.to_string n
    | String s -> if readable then "\"" ^ String.escaped s ^ "\"" else s

  let to_bool = function
    | Bool b -> b
    | Nil -> false
    (* …or /maybe/ do like python, and make empty strings and zero falsy? *)
    | Number _ | String _ -> true

  let to_float = function Number n -> Ok n | _ -> error "Not a number"

  let compare l r =
    let cmp_variant_of_int n =
      Ok
        (if n > 0 then `Greater_than
         else if n < 0 then `Less_than
         else if n = 0 then `Equal
         else `Incomparable)
    in
    match (l, r) with
    | Nil, Nil -> Ok `Equal
    | Bool l, Bool r -> Bool.compare l r |> cmp_variant_of_int
    | Number l, Number r -> Float.compare l r |> cmp_variant_of_int
    | String l, String r -> String.compare l r |> cmp_variant_of_int
    | _, _ -> Ok `Incomparable

  let ( + ) =
    Fun.curry (function
      | Number l, Number r -> Ok (Number (l +. r))
      | String l, String r -> Ok (String (l ^ r))
      | l, r -> error [%string "can't add %{to_string l} with %{to_string r}"])
end

module Env = struct
  module Map = Map.Make (Ast.Id)

  type bindings = Obj.t ref Map.t
  type t = { bindings : bindings; stdout : Eio.Flow.sink_ty r }

  let of_eio_env env =
    {
      bindings = Map.empty;
      stdout = (Eio.Stdenv.stdout env :> Eio.Flow.sink_ty r);
    }

  let with_binding key value { bindings; stdout } =
    { stdout; bindings = bindings |> Map.add key (ref value) }

  let get key { bindings; _ } =
    match Map.get key bindings with
    | Some ref -> Ok !ref
    | None ->
        error
          [%string
            "tried to reference unbound variable %{Ast.Id.to_string key}"]

  let set key value { bindings; _ } =
    match Map.get key bindings with
    | Some ref ->
        let prev = !ref in
        ref := value;
        Ok prev
    | None ->
        error [%string "tried to set unbound variable %{Ast.Id.to_string key}"]
end

let eval_binary_op _env op l r =
  let open Result.Infix in
  let do_arith f l r =
    let open Obj in
    let* l = to_float l and* r = to_float r in
    Ok (Obj.Number (f l r))
  in
  match op with
  | Ast.Equal -> (
      Obj.(compare l r >|= function `Equal -> Bool true | _ -> Bool false))
  | Ast.Not_equal -> (
      Obj.(compare l r >|= function `Equal -> Bool false | _ -> Bool true))
  | Ast.Less_than -> (
      Obj.(compare l r >|= function `Less_than -> Bool true | _ -> Bool false))
  | Ast.Less_equal -> (
      Obj.(
        compare l r >|= function
        | `Equal | `Less_than -> Bool true
        | _ -> Bool false))
  | Ast.Greater_than -> (
      Obj.(
        compare l r >|= function `Greater_than -> Bool true | _ -> Bool false))
  | Ast.Greater_equal -> (
      Obj.(
        compare l r >|= function
        | `Equal | `Greater_than -> Bool true
        | _ -> Bool false))
  | Ast.Plus -> Obj.(l + r)
  | Ast.Minus -> do_arith Float.( - ) l r
  | Ast.Times -> do_arith Float.( * ) l r
  | Ast.Div -> do_arith Float.( / ) l r
  | Ast.Comma -> Ok r

let eval_unary_op _env op value =
  match op with
  | Ast.Negate ->
      Result.Infix.(Obj.to_float value >|= Float.neg >|= fun n -> Obj.Number n)
  | Ast.Not -> Ok Obj.(Bool (not (to_bool value)))

let eval_literal =
  let open Obj in
  function
  | Ast.Number n -> Number n
  | Ast.String s -> String s
  | Ast.True -> Bool true
  | Ast.False -> Bool false
  | Ast.Nil -> Nil

let logic_op_condition = function Ast.Or -> Fun.id | Ast.And -> not

let rec eval_expr env (expr : Ast.expr) =
  let open Result.Infix in
  match expr with
  | Ast.Binary (l, op, r) ->
      let* l = eval_expr env l and* r = eval_expr env r in
      eval_binary_op env op l r
  | Ast.Unary (op, expr) ->
      let* value = eval_expr env expr in
      eval_unary_op env op value
  | Ast.Grouping expr -> eval_expr env expr
  | Ast.Literal lit -> Ok (eval_literal lit)
  | Ast.Variable id -> Env.get id env
  | Ast.Assign (Variable_p id, expr) ->
      let* value = eval_expr env expr in
      Env.set id value env
  | Ast.Logic (l, op, r) ->
      let* l = eval_expr env l in
      if Obj.to_bool l |> logic_op_condition op then Ok l else eval_expr env r

type t = Value of Obj.t | Binding of Ast.Id.t * Obj.t * Env.t | Void

let rec eval_while env cond body =
  let open Result.Infix in
  let* cond' = eval_expr env cond in
  if Obj.to_bool cond' then
    let* _ = eval_stmt env body in
    eval_while env cond body
  else Ok Void

and eval_stmt env stmt =
  let open Result.Infix in
  match stmt with
  | Ast.Expr e -> eval_expr env e >|= fun o -> Value o
  | Ast.Log exprs ->
      let stdout = env.Env.stdout in
      let+ strs =
        exprs
        |> Result.map_l (fun e ->
               eval_expr env e >|= Obj.to_string ~readable:false)
      in
      stdout |> Eio.Flow.copy_string (String.concat ~sep:" " strs ^ "\n");
      Void
  | Ast.If { condition; if_true; if_false } ->
      let* cond = eval_expr env condition in
      if Obj.to_bool cond then eval_stmt env if_true
      else Option.map_or ~default:(Ok Void) (eval_stmt env) if_false
  | Ast.Block stmts -> eval_block env stmts
  | Ast.While { condition; body } -> eval_while env condition body

and eval_decl env decl =
  let open Result.Infix in
  match decl with
  | Ast.Var (id, value) ->
      let+ value = eval_expr env value in
      let env = Env.with_binding id value env in
      Binding (id, value, env)
  | Ast.Stmt stmt -> eval_stmt env stmt

(* The type difference between this and [eval] is on purpose;
   making them the same doesn't work. *)
and eval_block env decls =
  let open Result.Infix in
  let f (env, acc) decl =
    let+ result = eval_decl env decl in
    match result with
    | (Value _ | Void) as not_bind -> (env, not_bind :: acc)
    | Binding (_, _, env) as bind -> (env, bind :: acc)
  in
  let+ _, values = Result.fold_l f (env, []) decls in
  values |> List.head_opt |> Option.value ~default:(Value Obj.Nil)

let eval env ast =
  let open Result.Infix in
  let f (env, acc) decl =
    let+ result = eval_decl env decl in
    match result with
    | (Value _ | Void) as not_bind -> (env, not_bind :: acc)
    | Binding (_, _, env) as bind -> (env, bind :: acc)
  in
  let+ env, ret = Result.fold_l f (env, []) ast in
  (env, List.rev ret)
