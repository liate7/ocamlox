open! ContainersLabels
open! Eio.Std
open Result.Infix

let eval_string _env str =
  let buf = Sedlexing.Utf8.from_string str in
  let gen () =
    match Lexer.read buf with
    | exception Lexer.SyntaxError err -> Some (Error (`Syntax err))
    | EOF -> None
    | tok -> Some (Ok tok)
  in
  Seq.of_gen gen |> List.of_seq |> Result.flatten_l

let error_to_string = function
  | `Msg str -> "Error: " ^ str
  | `Syntax str -> "Lexing error: " ^ str

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
  let f tok = Eio.Flow.copy_string (Token.to_string tok ^ "\n") stdout in
  List.iter ~f result

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
