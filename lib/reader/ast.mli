(* TODO: separate out the signature Ast_type, so no need for [module type of]? *)
include module type of Ast_type

val to_string : (literal, place) t -> string
