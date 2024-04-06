open Reader

type id = Id.t * int option
type t = (Ast.literal, id) Ast.t
type input = (Ast.literal, Id.t) Ast.t

val go : input list -> (t list, [> Error.t ]) result
