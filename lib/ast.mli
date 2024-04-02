(* TODO: separate out the signature Ast_type, so no need for [module type of]? *)
include module type of Ast_type

val to_string : t -> string

type error = [ `Syntax of string | `Parsing of string ]

val of_lexbuf : Sedlexing.lexbuf -> (t option, [> error ]) result
(** Main parsing function: takes a lexbuf and returns
    either the result of parsing it or a syntax error  *)
