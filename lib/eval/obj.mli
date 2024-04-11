open Reader

module Attr : sig
  type 'value t

  val create : int -> _ t
  val of_seq : (Id.t * 'value) Seq.t -> 'value t
  val find_opt : 'value t -> Id.t -> 'value option
end

type t =
  | Nil
  | Bool of bool
  | Number of float
  | String of string
  | Builtin of Id.t * int * (Auth.t -> t list -> (t, string) result)
  | Function of Id.t option * func
  | Class of klass
  | Object of klass * t Attr.t

and func = {
  arity : int;
  params : Id.t list;
  env : t Dict.t;
  body : (Ast.literal, Resolver.place) Ast.stmt;
}

and klass = {
  name : Id.t;
  superclass : klass option;
  init : func option;
  methods : func Attr.t;
}

val to_string : ?readable:bool -> t -> string
val to_bool : t -> bool
val to_float : t -> (float, [> Error.t ]) result
val compare : t -> t -> [> `Greater_than | `Less_than | `Equal | `Incomparable ]
val ( + ) : t -> t -> (t, [> Error.t ]) result
val get : ?super:t -> t -> Id.t -> (t, [> Error.t ]) result
val set : t -> Id.t -> t -> (t, [> Error.t ]) result
val bind : t -> func -> t
