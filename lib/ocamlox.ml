open! ContainersLabels
open! Eio.Std
open Result.Infix
module I = Parser.MenhirInterpreter

let lexing_pos_to_string ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  let line = Int.to_string pos_lnum
  and col = Int.to_string (pos_cnum - pos_bol) in
  [%string "line %{line}, column %{col}"]

let rec parse lexbuf chkpoint =
  match chkpoint with
  | I.InputNeeded _ ->
      let token = Lexer.read lexbuf
      and startp, endp = Sedlexing.lexing_positions lexbuf in
      parse lexbuf @@ I.offer chkpoint (token, startp, endp)
  | I.Shifting _ | I.AboutToReduce _ -> parse lexbuf @@ I.resume chkpoint
  | I.HandlingError _ ->
      let start, _ = Sedlexing.lexing_positions lexbuf in
      Error (`Syntax [%string "Syntax error at %{lexing_pos_to_string start}"])
  | I.Accepted ast -> Ok ast
  | I.Rejected -> assert false

let eval_string _env str =
  let buf = Sedlexing.Utf8.from_string str in
  let startp, _ = Sedlexing.lexing_positions buf in
  try parse buf @@ Parser.Incremental.prog startp
  with Lexer.SyntaxError msg -> Error (`Syntax msg)

let error_to_string = function
  | `Msg str -> "Error: " ^ str
  | `Syntax str -> "Parsing error: " ^ str

let eval_file path env =
  let file =
    if Filename.is_relative path then Eio.Path.(Eio.Stdenv.cwd env / path)
    else Eio.Path.(Eio.Stdenv.fs env / path)
  in
  Eio.Path.with_open_in file Eio.Buf_read.(parse take_all ~max_size:Int.max_int)
  >>= eval_string env
  |> function
  | Ok _ -> Ok 0
  | Error err -> Error (error_to_string err)

let repl_print stdout result =
  match result with
  | Some ast -> Eio.Flow.copy_string (Ast.to_string ast ^ "\n") stdout
  | None -> assert false

let repl env =
  (* Can't just use [Buf_read.parse] bcs always parses until EOF -_-.
     This seems to be the easiest way to…not do that *)
  let stdin = Eio.Stdenv.stdin env |> Eio.Buf_read.of_flow ~max_size:Int.max_int
  and stdout = Eio.Stdenv.stdout env in
  let rec loop () =
    Eio.Flow.copy_string "> " stdout;
    if not @@ Eio.Buf_read.at_end_of_input stdin then (
      let* line = Eio.Buf_read.(format_errors line) stdin in
      let* result = eval_string env line in
      repl_print stdout result;
      loop ())
    else Ok 0
  in
  loop () |> Result.map_err error_to_string
