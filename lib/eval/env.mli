open Reader

type t

val of_eio_env :
  < stdout : [> Eio.Flow.sink_ty ] Eio.Resource.t
  ; clock : [> float Eio.Time.clock_ty ] Eio.Resource.t
  ; .. > ->
  t

val of_function_env : t -> Obj.t Dict.t -> t
val define : Id.t -> Obj.t -> t -> t
val get : Id.t * int option -> t -> (Obj.t, [> Error.t ]) result
val set : Id.t * int option -> Obj.t -> t -> (Obj.t, [> Error.t ]) result
val new_scope : t -> t
val auth : t -> Auth.t
val bindings : t -> Obj.t Dict.t
