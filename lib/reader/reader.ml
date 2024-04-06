(* Module Exports *)
module Ast = Ast
module Id = Ast.Id

(* Actual interface *)
open! ContainersLabels
module I = Parser.MenhirInterpreter
open Ast

type checkpoint = (literal, Id.t) t list I.checkpoint

type error =
  [ `Syntax of string | `Parsing of string | `Needs_input of checkpoint ]

type 'a parser =
  Sedlexing.lexbuf -> ((literal, Id.t) t list, ([> error ] as 'a)) result

let lexing_pos_to_string ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  let line = Int.to_string pos_lnum
  and col = Int.to_string (pos_cnum - pos_bol) in
  [%string "line %{line}, column %{col}"]

let parse_from_checkpoint checkpoint lexbuf =
  let rec loop token checkpoint =
    match checkpoint with
    | I.InputNeeded _ -> (
        let token = Lexer.read lexbuf
        and startp, endp = Sedlexing.lexing_positions lexbuf in
        match token with
        | Parser.EOF ->
            if I.acceptable checkpoint token startp then
              loop (Some token) @@ I.offer checkpoint (token, startp, endp)
            else Error (`Needs_input checkpoint)
        | token -> loop (Some token) @@ I.offer checkpoint (token, startp, endp)
        )
    | I.Shifting _ | I.AboutToReduce _ -> loop token @@ I.resume checkpoint
    | I.HandlingError _ ->
        let start, _ = Sedlexing.lexing_positions lexbuf
        and lexeme = Sedlexing.Utf8.lexeme lexbuf
        and last_token =
          match token with
          | None -> "at start"
          | Some tok -> [%string "last token: %{Token.to_string tok}"]
        in
        Error
          (`Syntax
            [%string
              "Syntax error at %{lexing_pos_to_string start} (%{lexeme}) \
               (%{last_token})"])
    | I.Accepted ast -> Ok ast
    | I.Rejected ->
        (* should only happen after a [I.HandlingError] event,
           which we never continue from *)
        assert false
  in
  try loop None checkpoint with Lexer.SyntaxError msg -> Error (`Parsing msg)

let parse lexbuf =
  let startp, _ = Sedlexing.lexing_positions lexbuf in
  parse_from_checkpoint (Parser.Incremental.program startp) lexbuf
