open! ContainersLabels
open Reader

type input = Resolver.t
type t = Value of Obj.t | Binding of Id.t * Obj.t * Env.t | Void

let eval_binary_op _env op l r =
  let open Result.Infix in
  let do_arith f l r k =
    let open Obj in
    let* l = to_float l and* r = to_float r in
    Ok (k (f l r))
  and number n = Obj.Number n
  and bool b = Obj.Bool b in
  match op with
  | Ast.Equal -> Ok (Obj.equal l r |> bool)
  | Ast.Not_equal -> Ok (Obj.equal l r |> not |> bool)
  | Ast.Less_than -> do_arith Float.( < ) l r bool
  | Ast.Less_equal -> do_arith Float.( <= ) l r bool
  | Ast.Greater_than -> do_arith Float.( > ) l r bool
  | Ast.Greater_equal -> do_arith Float.( >= ) l r bool
  | Ast.Plus -> Obj.(l + r)
  | Ast.Minus -> do_arith Float.( - ) l r number
  | Ast.Times -> do_arith Float.( * ) l r number
  | Ast.Div -> do_arith Float.( / ) l r number
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

let rec eval_body env decls =
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
  let arity_mismatch expected actual =
    Error.of_string
      [%string
        "arity mismatch: expected %{Int.to_string expected} args, got \
         %{Int.to_string @@ List.length actual}"]
  in
  match f with
  | Builtin (_, arity, f) when arity = List.length args ->
      f (Env.auth env) args |> Result.map_err (fun s -> `Error s)
  | Function (_, { arity; params; env = closure; body })
    when arity = List.length args -> (
      let env =
        List.fold_left2 params args
          ~init:Env.(of_function_env env closure |> new_scope)
          ~f:(fun env name value -> Env.define name value env)
      in
      match eval_stmt env body with
      | Ok _ -> Ok Obj.Nil
      | Error (`Return ret) -> Ok ret
      | Error _ as err -> err)
  | Class ({ init; _ } as c) -> (
      let obj = Object (c, Attr.create 4) in
      match init with
      | Some init ->
          eval_apply env (Obj.bind obj init @@ Id.of_string "init") args
      | None when List.length args = 0 -> Ok obj
      | None -> arity_mismatch 0 args)
  | Builtin (_, arity, _) | Function (_, { arity; _ }) ->
      arity_mismatch arity args
  | _ -> Error.of_string "can't call this; can only call functions and classes"

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
  | Ast.Get (Resolver.Var var) -> Env.get var env
  | Ast.Get (Resolver.Field (expr, attr)) ->
      let* from = eval_expr env expr in
      Obj.get from attr
  | Ast.Get (Resolver.Super { super; this; attr }) ->
      let* this = Env.get this env and* super = Env.get super env in
      Obj.get ~super this attr
  | Ast.Assign (Resolver.Var var, expr) ->
      let* value = eval_expr env expr in
      Env.set var value env
  | Ast.Assign (Resolver.Field (obj, attr), expr) ->
      let* obj = eval_expr env obj and* expr = eval_expr env expr in
      Obj.set obj attr expr
  | Ast.Assign (Resolver.Super { super = _; this; attr }, expr) ->
      let* obj = Env.get this env and* expr = eval_expr env expr in
      Obj.set obj attr expr
  | Ast.Logic (l, op, r) ->
      let* l = eval_expr env l in
      if Obj.to_bool l |> logic_op_condition op then Ok l else eval_expr env r
  | Ast.Call (f, args) ->
      let* f = eval_expr env f in
      let* args = Result.map_l (eval_expr env) args in
      eval_apply env f args
  | Ast.Lambda { params; body } ->
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
  | Ast.Var (id, value) ->
      let+ value = eval_expr env value in
      let env = Env.define id value env in
      Binding (id, value, env)
  | Ast.Stmt stmt -> eval_stmt env stmt
  | Ast.Fun (id, { params; body }) ->
      let func =
        Obj.Function
          ( Some id,
            { arity = List.length params; params; env = Env.bindings env; body }
          )
      in
      let env = Env.define id func env in
      Ok (Binding (id, func, env))
  | Ast.Class { name; superclass; methods } ->
      let+ superclass =
        Result.opt_map
          (function
            | Resolver.Var name -> (
                Env.get name env >>= function
                | Class c -> Ok c
                | _ -> Error.of_string "superclass must be a class")
            | _ -> assert false)
          superclass
      in
      let method_env =
        match superclass with
        | None -> env
        | Some super ->
            Env.new_scope env |> Env.define (Id.of_string "super") (Class super)
      in
      let methods =
        List.to_seq methods
        |> Seq.map (fun (name, { Ast.params; body }) ->
               ( name,
                 {
                   Obj.arity = List.length params;
                   params;
                   body;
                   env = Env.bindings method_env;
                 } ))
        |> Obj.Attr.of_seq
      in
      let init =
        let open Option.Infix in
        Obj.Attr.find_opt methods @@ Id.of_string "init"
        |> Option.or_lazy ~else_:(fun () ->
               superclass >>= fun { init; _ } -> init)
      in
      let klass = Obj.Class { name; superclass; init; methods } in
      let env = Env.define name klass env in
      Binding (name, klass, env)

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
