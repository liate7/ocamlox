open Reader

type input = Resolver.t
type t = Value of Obj.t | Binding of Id.t * Obj.t * Env.t | Void

val go :
  Env.t ->
  (Ast.literal, Resolver.place) Ast.t list ->
  (Env.t * t list, [> Error.t | `Return of Obj.t ]) result
