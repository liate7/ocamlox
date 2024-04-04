(* TODO: separate out the signature Ast_type, so no need for [module type of]? *)
include module type of Ast_type

val to_string : t -> string

type checkpoint

type error =
  [ `Syntax of string | `Parsing of string | `Needs_input of checkpoint ]
(** Needs_input errors can be continued by passing the checkpoint and
    a new lexbuf to [parse_from_checkpoint]. *)

type 'a parser = Sedlexing.lexbuf -> (t list, ([> error ] as 'a)) result

val parse : 'a parser
(** Main parsing function. *)

val parse_from_checkpoint : checkpoint -> 'a parser
(** Primarily for continuing a previous parse that returned [`Needs_input _].
    See [error] documentation for details. *)
