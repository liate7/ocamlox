open Reader

type error = [ `Error of string ]

module Obj : sig
  type t

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
  Env.t ->
  (Ast.literal, Id.t) Ast.t list ->
  (Env.t * t list, [> error | `Return of Obj.t ]) result
