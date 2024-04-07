open! ContainersLabels
open Eio.Std
open Reader

type t = { bindings : Obj.t Dict.t; auth : Auth.t }

let auth t = t.auth
let bindings t = t.bindings

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
    bindings = globals;
    auth =
      {
        stdout = (Eio.Stdenv.stdout env :> Eio.Flow.sink_ty r);
        clock = (Eio.Stdenv.clock env :> float Eio.Time.clock_ty r);
      };
  }

let of_function_env env bindings = { bindings; auth = env.auth }

let define key value ({ bindings; _ } as t) =
  (* traceln "%s" *)
  (*   [%string *)
  (* "defined %{Id.to_string key} as %{Obj.to_string value} (height \ *)
     (*      %{Dict.height bindings |> Int.to_string })"]; *)
  Dict.add bindings key value;
  t

let get (key, depth) { bindings; _ } =
  (* traceln "%s" *)
  (*   [%string *)
  (*     {|getting %{Id.to_string key} (depth %{ Option.map_or ~default:"global" Int.to_string depth })|}]; *)
  Dict.find_opt ~depth bindings key
  |> Option.to_result
       (`Error
         [%string "tried to reference unbound variable %{Id.to_string key}"])

let set (key, depth) value { bindings; _ } =
  Dict.replace ~depth bindings key value
  |> Option.to_result
       (`Error [%string "tried to set unbound variable %{Id.to_string key}"])

let new_scope { bindings; auth } = { bindings = Dict.child_of bindings; auth }
