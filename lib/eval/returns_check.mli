open Reader

val go : ('lit, 'id) Ast.t list -> (('lit, 'id) Ast.t list, [> Error.t ]) result
