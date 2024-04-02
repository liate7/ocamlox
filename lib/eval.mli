type error = [ `Error of string ]

module Obj : sig
  type t = Nil | Bool of bool | Number of float | String of string

  val to_string : t -> string
end

val eval : Eio_linux.stdenv -> Ast.t option -> (Obj.t, [> error ]) result
