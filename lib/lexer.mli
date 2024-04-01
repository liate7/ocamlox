exception SyntaxError of string

open Sedlexing

val read : lexbuf -> Parser.token
