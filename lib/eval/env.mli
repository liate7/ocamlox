type t

val of_eio_env :
  < stdout : [> Eio.Flow.sink_ty ] Eio.Resource.t
  ; clock : [> float Eio.Time.clock_ty ] Eio.Resource.t
  ; .. > ->
  t

val of_function_env : t -> Obj.t Dict.t -> t
val define : Resolver.id -> Obj.t -> t -> t
val get : Resolver.id -> t -> (Obj.t, [> Error.t ]) result
val set : Resolver.id -> Obj.t -> t -> (Obj.t, [> Error.t ]) result
val new_scope : t -> t
val auth : t -> Auth.t
val bindings : t -> Obj.t Dict.t
