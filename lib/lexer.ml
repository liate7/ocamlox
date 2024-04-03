open Parser
open Sedlexing

exception SyntaxError of string

(* Numbers *)
let digit = [%sedlex.regexp? '0' .. '9']
let frac = [%sedlex.regexp? '.', Plus digit]
let exponent = [%sedlex.regexp? ('e' | 'E'), Opt ('-' | '+'), Plus digit]

let float =
  [%sedlex.regexp?
    Opt '-', (Plus digit, Opt frac, Opt exponent | "inf" | "nan")]

(* Other more complicated things *)
let identifier = [%sedlex.regexp? (id_start | '_'), Star id_continue]
let comment = [%sedlex.regexp? "//", Star (Compl nl)]

let rec read lexbuf =
  match%sedlex lexbuf with
  | white_space -> read lexbuf
  | nl | comment ->
      new_line lexbuf;
      read lexbuf
  | float -> NUMBER (float_of_string @@ Utf8.lexeme lexbuf)
  | "and" -> AND
  | "class" -> CLASS
  | "false" -> FALSE
  | "defn" -> DEFN
  | "for" -> FOR
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "log" -> LOG
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "let" -> LET
  | "while" -> WHILE
  | '"' -> read_string (Buffer.create 17) lexbuf
  | identifier -> IDENTIFIER (Utf8.lexeme lexbuf)
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  | '{' -> LEFT_BRACE
  | '}' -> RIGHT_BRACE
  | ',' -> COMMA
  | '.' -> DOT
  | '+' -> PLUS
  | '-' -> MINUS
  | ';' -> SEMICOLON
  | '/' -> SLASH
  | '*' -> STAR
  | '!' -> BANG
  | "!=" -> BANG_EQUAL
  | '=' -> EQUAL
  | "==" -> EQUAL_EQUAL
  | ">" -> GREATER
  | ">=" -> GREATER_EQUAL
  | "<" -> LESS
  | "<=" -> LESS_EQUAL
  | eof -> EOF
  | _ -> raise (SyntaxError ("Unexpected char: " ^ Utf8.lexeme lexbuf))

and read_string buf lexbuf =
  let char_error () =
    raise (SyntaxError ("Illegal string character: " ^ Utf8.lexeme lexbuf))
  in
  let read_escape () =
    match%sedlex lexbuf with
    | 'b' -> '\b'
    | 'f' -> '\012'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '/' -> '/'
    | _ -> char_error ()
  in
  match%sedlex lexbuf with
  | '"' -> STRING (Buffer.contents buf)
  | '\\' ->
      Buffer.add_char buf @@ read_escape ();
      read_string buf lexbuf
  | Plus (Compl ('"' | '\\')) ->
      Buffer.add_string buf @@ Utf8.lexeme lexbuf;
      read_string buf lexbuf
  | eof -> raise (SyntaxError "Unterminated string")
  | _ -> char_error ()
