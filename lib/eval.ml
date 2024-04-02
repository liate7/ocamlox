open! ContainersLabels

type error = [ `Error of string ]

let error msg = Error (`Error msg)

module Obj = struct
  type t = Nil | Bool of bool | Number of float | String of string

  let to_string = function
    | Nil -> "nil"
    | Bool true -> "true"
    | Bool false -> "false"
    | Number n ->
        if Int.of_float n |> Float.of_int =. n then
          Int.(of_float n |> to_string)
        else Float.to_string n
    | String s -> "\"" ^ String.escaped s ^ "\""

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
      | l, r ->
          error [%string "Error: can't add %{to_string l} with %{to_string r}"])
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

let eval env ast =
  match ast with Some ast -> eval_expr env ast | None -> Ok Obj.Nil
