open Reader

type t =
  | Nil
  | Bool of bool
  | Number of float
  | String of string
  | Builtin of Resolver.id * int * (Auth.t -> t list -> (t, string) result)
  | Function of Resolver.id option * func

and func = {
  arity : int;
  params : Resolver.id list;
  env : t Dict.t;
  body : (Ast.literal, Resolver.id) Ast.stmt;
}

val to_string : ?readable:bool -> t -> string
val to_bool : t -> bool
val to_float : t -> (float, [> Error.t ]) result
val compare : t -> t -> [> `Greater_than | `Less_than | `Equal | `Incomparable ]
val ( + ) : t -> t -> (t, [> Error.t ]) result
