open! ContainersLabels
open Reader

type input = Resolver.t
type t = Value of Obj.t | Binding of Id.t * Obj.t * Env.t | Void

let eval_binary_op _env op l r =
  let open Result.Infix in
  let do_arith f l r =
    let open Obj in
    let* l = to_float l and* r = to_float r in
    Ok (Obj.Number (f l r))
  in
  match op with
  | Ast.Equal ->
      Ok Obj.(compare l r |> function `Equal -> Bool true | _ -> Bool false)
  | Ast.Not_equal ->
      Ok Obj.(compare l r |> function `Equal -> Bool false | _ -> Bool true)
  | Ast.Less_than ->
      Ok
        Obj.(
          compare l r |> function `Less_than -> Bool true | _ -> Bool false)
  | Ast.Less_equal ->
      Ok
        Obj.(
          compare l r |> function
          | `Equal | `Less_than -> Bool true
          | _ -> Bool false)
  | Ast.Greater_than ->
      Ok
        Obj.(
          compare l r |> function `Greater_than -> Bool true | _ -> Bool false)
  | Ast.Greater_equal ->
      Ok
        Obj.(
          compare l r |> function
          | `Equal | `Greater_than -> Bool true
          | _ -> Bool false)
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
    Error.of_string
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
      f (Env.auth env) args |> Result.map_err (fun s -> `Error s)
  | Builtin (_, arity, _) ->
      Error.of_string
        [%string
          "arity mismatch: expected %{Int.to_string arity} args, got \
           %{Int.to_string @@ List.length args}"]
  | Function (_, f) -> non_builtin_apply env f args
  | _ -> Error.of_string "can't call this; can only call functions"

and eval_expr env expr =
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
             {
               arity = List.length params;
               params;
               env = Env.bindings env;
               body;
             } ))

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
      let stdout = (Env.auth env).stdout in
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
  | Ast.Var (((id', _) as id), value) ->
      let+ value = eval_expr env value in
      let env = Env.define id value env in
      Binding (id', value, env)
  | Ast.Stmt stmt -> eval_stmt env stmt
  | Ast.Fun (((id', _) as id), params, body) ->
      let func =
        Obj.Function
          ( Some id,
            { arity = List.length params; params; env = Env.bindings env; body }
          )
      in
      let env = Env.define id func env in
      Ok (Binding (id', func, env))

(* The type difference between this and [go] is on purpose;
   making them the same doesn't work. *)
and eval_block env decls =
  let open Result.Infix in
  eval_body (Env.new_scope env) decls
  >|= List.head_opt
  >|= Option.value ~default:(Value Obj.Nil)

let go env ast =
  let open Result.Infix in
  let f (env, acc) decl =
    let+ result = eval_decl env decl in
    match result with
    | (Value _ | Void) as not_bind -> (env, not_bind :: acc)
    | Binding (_, _, env) as bind -> (env, bind :: acc)
  in
  let+ env, ret = Result.fold_l f (env, []) ast in
  (env, List.rev ret)
