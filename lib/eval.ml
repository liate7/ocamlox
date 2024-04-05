open! ContainersLabels
open! Eio.Std
module Id = Ast.Id

type error = [ `Error of string ]

let error msg = Error (`Error msg)

type auth = { stdout : Eio.Flow.sink_ty r; clock : float Eio.Time.clock_ty r }

module Dict = Hashtbl.Make (Id)

module Obj = struct
  type t =
    | Nil
    | Bool of bool
    | Number of float
    | String of string
    | Builtin of Id.t * int * (auth -> t list -> (t, string) result)
    | Function of Id.t option * func

  and func = {
    arity : int;
    params : Id.t list;
    env : t Dict.t * t Dict.t list;
    body : Ast.stmt;
  }

  let to_string ?(readable = true) = function
    | Nil -> "nil"
    | Bool true -> "true"
    | Bool false -> "false"
    | Number n ->
        if Int.of_float n |> Float.of_int =. n then
          Int.(of_float n |> to_string)
        else Float.to_string n
    | String s -> if readable then "\"" ^ String.escaped s ^ "\"" else s
    | Builtin (name, arity, _) ->
        let params_str = List.replicate arity "_" |> String.concat ~sep:", " in
        [%string "#<builtin %{Id.to_string name}(%{params_str})>"]
    | Function (name, { params; _ }) ->
        let params_str =
          params |> List.map ~f:Id.to_string |> String.concat ~sep:", "
        in
        [%string
          {|#<function %{Option.map_or ~default:"<anon>" Id.to_string name}(%{params_str})>|}]

  let to_bool = function
    | Bool b -> b
    | Nil -> false
    (* …or /maybe/ do like python, and make empty strings and zero falsy? *)
    | Number _ | String _ | Builtin _ | Function _ -> true

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
  type bindings = Obj.t Dict.t * Obj.t Dict.t list
  type t = { bindings : bindings; auth : auth }

  let globals =
    let builtin name arity fn =
      let id = Id.of_string name in
      (id, Obj.Builtin (id, arity, fn))
    in
    Dict.of_seq
    @@ Seq.of_list
         [
           builtin "clock" 0 (fun auth _ ->
               Ok (Obj.Number (Eio.Time.now auth.clock)));
         ]

  let of_eio_env env =
    {
      bindings = (globals, []);
      auth =
        {
          stdout = (Eio.Stdenv.stdout env :> Eio.Flow.sink_ty r);
          clock = (Eio.Stdenv.clock env :> float Eio.Time.clock_ty r);
        };
    }

  let of_function_env env bindings = { bindings; auth = env.auth }

  let define key value ({ bindings = cur, _; _ } as t) =
    Dict.replace cur key value;
    t

  let get key { bindings; _ } =
    let rec loop (cur, parents) =
      match (Dict.find_opt cur key, parents) with
      | Some obj, _ -> Ok obj
      | None, next :: rest -> loop (next, rest)
      | None, [] ->
          error
            [%string "tried to reference unbound variable %{Id.to_string key}"]
    in
    loop bindings

  let set key value { bindings; _ } =
    let rec loop (cur, parents) =
      match (Dict.find_opt cur key, parents) with
      | Some obj, _ ->
          Dict.replace cur key value;
          Ok obj
      | None, next :: rest -> loop (next, rest)
      | None, [] ->
          error [%string "tried to set unbound variable %{Id.to_string key}"]
    in
    loop bindings

  let new_scope { bindings = cur, parents; auth } =
    { bindings = (Dict.create 2, cur :: parents); auth }
end

type t = Value of Obj.t | Binding of Id.t * Obj.t * Env.t | Void

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

let rec non_builtin_apply env { Obj.arity; params; env = closure; body } args =
  if List.length args <> arity then
    error
      [%string
        "arity mismatch: expected %{Int.to_string arity} args, got \
         %{Int.to_string @@ List.length args}"]
  else
    let env =
      List.fold_left2
        ~init:Env.(of_function_env env closure |> new_scope)
        params args
        ~f:(fun env name value -> Env.define name value env)
    in
    match eval_stmt env body with
    | Ok _ -> Ok Obj.Nil
    | Error (`Return ret) -> Ok ret
    | Error _ as err -> err

and eval_body env decls =
  let open Result.Infix in
  let f (env, acc) decl =
    let+ result = eval_decl env decl in
    match result with
    | (Value _ | Void) as not_bind -> (env, not_bind :: acc)
    | Binding (_, _, env) as bind -> (env, bind :: acc)
  in
  let+ _, values = Result.fold_l f (env, []) decls in
  values

and eval_apply env f args =
  let open Obj in
  match f with
  | Builtin (_, arity, f) when arity = List.length args ->
      f env.Env.auth args |> Result.map_err (fun s -> `Error s)
  | Builtin (_, arity, _) ->
      error
        [%string
          "arity mismatch: expected %{Int.to_string arity} args, got \
           %{Int.to_string @@ List.length args}"]
  | Function (_, f) -> non_builtin_apply env f args
  | _ -> error "can't call this; can only call functions"

and eval_expr env (expr : Ast.expr) =
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
  | Ast.Call (f, args) ->
      let* f = eval_expr env f in
      let* args = Result.map_l (eval_expr env) args in
      eval_apply env f args
  | Ast.Lambda (params, body) ->
      Ok
        (Obj.Function
           ( None,
             { arity = List.length params; params; env = env.bindings; body } ))

and eval_while env cond body =
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
      let stdout = env.Env.auth.stdout in
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
  | Ast.Return expr ->
      let* result =
        expr |> Option.map_or ~default:(Ok Obj.Nil) (eval_expr env)
      in
      Error (`Return result)

and eval_decl env decl =
  let open Result.Infix in
  match decl with
  | Ast.Var (id, value) ->
      let+ value = eval_expr env value in
      let env = Env.define id value env in
      Binding (id, value, env)
  | Ast.Stmt stmt -> eval_stmt env stmt
  | Ast.Fun (id, params, body) ->
      let func =
        Obj.Function
          ( Some id,
            { arity = List.length params; params; env = env.bindings; body } )
      in
      let env = Env.define id func env in
      Ok (Binding (id, func, env))

(* The type difference between this and [eval] is on purpose;
   making them the same doesn't work. *)
and eval_block env decls =
  let open Result.Infix in
  eval_body (Env.new_scope env) decls
  >|= List.head_opt
  >|= Option.value ~default:(Value Obj.Nil)

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
