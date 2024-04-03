open! ContainersLabels
open! Eio.Std
open Result.Infix

let eval_string env str =
  Sedlexing.Utf8.from_string str |> Ast.of_lexbuf >>= Eval.eval env

let error_to_string = function
  | `Msg str -> "Error: " ^ str
  | `Parsing str -> "Parsing error: " ^ str
  | `Syntax str -> str
  | `Error str -> "Evaluation error: " ^ str

let eval_file path env =
  let file =
    if Filename.is_relative path then Eio.Path.(Eio.Stdenv.cwd env / path)
    else Eio.Path.(Eio.Stdenv.fs env / path)
  and env = Eval.Env.of_eio_env env in
  Eio.Path.with_open_in file Eio.Buf_read.(parse take_all ~max_size:Int.max_int)
  >>= eval_string env
  |> function
  | Ok _ -> Ok 0
  | Error err -> Error (error_to_string err)

let repl_print stdout result =
  let printer = function
    | Eval.Value obj ->
        Eio.Flow.copy_string [%string "// _ = %{Eval.Obj.to_string obj}\n"]
          stdout
    | Eval.Binding (id, obj, _) ->
        Eio.Flow.copy_string
          [%string "// %{Ast.Id.to_string id} = %{Eval.Obj.to_string obj}\n"]
          stdout
    | Eval.Void -> ()
  in
  List.iter result ~f:printer

let repl env =
  (* Can't just use [Buf_read.parse] bcs always parses until EOF -_-.
     This seems to be the easiest way to…not do that *)
  let stdin = Eio.Stdenv.stdin env |> Eio.Buf_read.of_flow ~max_size:Int.max_int
  and stdout = Eio.Stdenv.stdout env in
  let env = Eval.Env.of_eio_env env in
  let rec loop env =
    Eio.Flow.copy_string "> " stdout;
    if not @@ Eio.Buf_read.at_end_of_input stdin then (
      let* line = Eio.Buf_read.(format_errors line) stdin in
      match eval_string env line with
      | Ok (env, result) ->
          repl_print stdout result;
          loop env
      | Error err ->
          Eio.Flow.copy_string (error_to_string err ^ "\n") stdout;
          loop env)
    else Ok 0
  in
  loop env |> Result.map_err error_to_string
