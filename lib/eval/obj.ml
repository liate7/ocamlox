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

and klass = {
  name : Id.t;
  superclass : klass option;
  init : func option;
  methods : func Attr.t;
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

let equal l r =
  match (l, r) with
  | Nil, Nil -> true
  | Bool l, Bool r -> Bool.equal l r
  | Number l, Number r -> Float.equal l r
  | String l, String r -> String.equal l r
  | Builtin (l_name, _, l_func), Builtin (r_name, _, r_func) ->
      Id.equal l_name r_name && Equal.physical l_func r_func
  | Function (_, l), Function (_, r) -> Equal.physical l r
  | Class l, Class r -> Equal.physical l r
  | Object (_, l), Object (_, r) -> Equal.physical l r
  | _, _ -> false

let ( + ) =
  Fun.curry (function
    | Number l, Number r -> Ok (Number (l +. r))
    | String l, String r -> Ok (String (l ^ r))
    | l, r ->
        Error.of_string [%string "can't add %{to_string l} with %{to_string r}"])

let bind inst { arity; params; env; body } name =
  let env = Dict.child_of env in
  Dict.add env (Id.of_string "this") inst;
  Function (Some name, { arity; params; env; body })

let get ?(super : t option) obj attr =
  let rec class_method { methods; superclass; _ } =
    match (Attr.find_opt methods attr, superclass) with
    | Some methd, _ -> Option.some @@ bind obj methd attr
    | None, Some super -> class_method super
    | None, None -> None
  in
  match (super, obj) with
  | Some (Class super), Object _ ->
      class_method super
      |> Option.map_or Result.return
           ~default:
             (Error.of_string
                [%string "undefined attribute %{Id.to_string attr}"])
  | Some super, Object _ ->
      Error.of_string
        [%string "%{to_string super} is not a class, how did you do this"]
  | None, Object (klass, attrs) ->
      Attr.find_opt attrs attr
      |> Option.or_lazy ~else_:(fun () -> class_method klass)
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
