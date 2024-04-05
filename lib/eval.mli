type error = [ `Error of string ]

module Obj : sig
  type t (* = *)

  (* | Nil *)
  (* | Bool of bool *)
  (* | Number of float *)
  (* | String of string *)
  (* | Builtin of Ast.Id.t * int * (auth -> t list -> (t, error) result) *)
  (* | Function of Ast.Id.t * int * unit *)

  val to_string : ?readable:bool -> t -> string
end

module Env : sig
  type t

  val of_eio_env :
    < stdout : [> Eio.Flow.sink_ty ] Eio.Resource.t
    ; clock : [> float Eio.Time.clock_ty ] Eio.Resource.t
    ; .. > ->
    t
end

type t = Value of Obj.t | Binding of Ast.Id.t * Obj.t * Env.t | Void

val eval :
  Env.t -> Ast.t list -> (Env.t * t list, [> error | `Return of Obj.t ]) result
