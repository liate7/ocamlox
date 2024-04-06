open! ContainersLabels
open Reader

type t =
  | Nil
  | Bool of bool
  | Number of float
  | String of string
  | Builtin of Resolver.id * int * (Auth.t -> t list -> (t, string) result)
  | Function of Resolver.id option * func

and func = {
  arity : int;
  params : Resolver.id list;
  env : t Dict.t;
  body : (Ast.literal, Resolver.id) Ast.stmt;
}

let to_string ?(readable = true) = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Number n ->
      if Int.of_float n |> Float.of_int =. n then Int.(of_float n |> to_string)
      else Float.to_string n
  | String s -> if readable then "\"" ^ String.escaped s ^ "\"" else s
  | Builtin (name, arity, _) ->
      let params_str = List.replicate arity "_" |> String.concat ~sep:", "
      and name, _ = name in
      [%string "#<builtin %{Id.to_string name}(%{params_str})>"]
  | Function (name, { params; _ }) ->
      let params_str =
        params
        |> List.map ~f:(fun (name, _) -> Id.to_string name)
        |> String.concat ~sep:", "
      and name_str =
        Option.map_or ~default:"<anon>"
          (fun (name, _) -> Id.to_string name)
          name
      in
      [%string {|#<function %{name_str}(%{params_str})>|}]

let to_bool = function
  | Bool b -> b
  | Nil -> false
  (* …or /maybe/ do like python, and make empty strings and zero falsy? *)
  | Number _ | String _ | Builtin _ | Function _ -> true

let to_float = function Number n -> Ok n | _ -> Error.of_string "Not a number"

let compare l r =
  let cmp_variant_of_int n =
    if n > 0 then `Greater_than
    else if n < 0 then `Less_than
    else if n = 0 then `Equal
    else `Incomparable
  in
  match (l, r) with
  | Nil, Nil -> `Equal
  | Bool l, Bool r -> Bool.compare l r |> cmp_variant_of_int
  | Number l, Number r -> Float.compare l r |> cmp_variant_of_int
  | String l, String r -> String.compare l r |> cmp_variant_of_int
  | _, _ -> `Incomparable

let ( + ) =
  Fun.curry (function
    | Number l, Number r -> Ok (Number (l +. r))
    | String l, String r -> Ok (String (l ^ r))
    | l, r ->
        Error.of_string [%string "can't add %{to_string l} with %{to_string r}"])
