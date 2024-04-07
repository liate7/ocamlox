open! ContainersLabels
open Reader
module Attr = Hashtbl.Make (Id)

type t =
  | Nil
  | Bool of bool
  | Number of float
  | String of string
  | Builtin of Id.t * int * (Auth.t -> t list -> (t, string) result)
  | Function of Id.t option * func
  | Class of klass
  | Object of klass * t Attr.t

and func = {
  arity : int;
  params : Id.t list;
  env : t Dict.t;
  body : (Ast.literal, Resolver.place) Ast.stmt;
}

and klass = { name : Id.t; init : func option; methods : func Attr.t }

let to_string ?(readable = true) = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Number n ->
      if Int.of_float n |> Float.of_int =. n then Int.(of_float n |> to_string)
      else Float.to_string n
  | String s -> if readable then "\"" ^ String.escaped s ^ "\"" else s
  | Builtin (name, arity, _) ->
      let params_str = List.replicate arity "_" |> String.concat ~sep:", " in
      [%string "#<builtin %{Id.to_string name}(%{params_str})>"]
  | Function (name, { params; _ }) ->
      let params_str =
        params |> List.map ~f:Id.to_string |> String.concat ~sep:", "
      and name_str = Option.map_or ~default:"<anon>" Id.to_string name in
      [%string {|#<function %{name_str}(%{params_str})>|}]
  | Class { name; _ } -> [%string {|#<class %{name |> Id.to_string}>|}]
  | Object ({ name; _ }, _) ->
      [%string {|#<class %{name |> Id.to_string} instance>|}]

let to_bool = function
  | Bool b -> b
  | Nil -> false
  (* …or /maybe/ do like python, and make empty strings and zero falsy? *)
  | Number _ | String _ | Builtin _ | Function _ | Class _ | Object _ -> true

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

let bind inst { arity; params; env; body } =
  let env = Dict.child_of env in
  Dict.add env (Id.of_string "this") inst;
  Function (None, { arity; params; env; body })

let get obj attr =
  match obj with
  | Object ({ methods; _ }, attrs) ->
      Attr.find_opt attrs attr
      |> Option.or_lazy ~else_:(fun () ->
             let open Option.Infix in
             let+ methd = Attr.find_opt methods attr in
             bind obj methd)
      |> Option.map_or Result.return
           ~default:
             (Error.of_string
                [%string "undefined attribute %{Id.to_string attr}"])
  | _ -> Error.of_string "only objects have attributes."

let set obj attr value =
  match obj with
  | Object (_, attrs) ->
      let ret = get obj attr |> Result.get_or ~default:Nil in
      Attr.replace attrs attr value;
      Ok ret
  | _ -> Error.of_string "only objects have attributes"
