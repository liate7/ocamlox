open Reader

type input = (Ast.literal, Ast.place) Ast.t
type t = (Ast.literal, Ast.place) Ast.t

val go : input list -> (t list, [> Error.t ]) result
