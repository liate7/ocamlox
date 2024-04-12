open! ContainersLabels
open! Eio.Std
open Result.Infix

let eval_string ?continue_from env str =
  let parse =
    match continue_from with
    | None -> Reader.parse
    | Some checkpoint -> Reader.parse_from_checkpoint checkpoint
  in
  Sedlexing.Utf8.from_string str |> parse >>= Eval.eval env

let error_to_string = function
  | `Msg str -> "Error: " ^ str
  | `Parsing str -> "Parsing error: " ^ str
  | `Needs_input _ -> "Parsing error: unexpected eof"
  | `Syntax str -> str
  | `Error str -> "Evaluation error: " ^ str
  | `Return _ -> "Evaluation error: tried to return from toplevel"

let eval_file flow env =
  let env = Eval.Env.of_eio_env env in
  Eio.Buf_read.(parse take_all ~max_size:Int.max_int) flow >>= eval_string env
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
          [%string "// %{Reader.Id.to_string id} = %{Eval.Obj.to_string obj}\n"]
          stdout
    | Eval.Void -> ()
  in
  List.iter result ~f:printer

let read_with_prompt input output prompt =
  Eio.Flow.copy_string prompt output;
  if Eio.Buf_read.at_end_of_input input then Ok None
  else Eio.Buf_read.(format_errors line) input >|= Option.some

let repl env =
  (* Can't just use [Buf_read.parse] bcs always parses until EOF -_-.
     This seems to be the easiest way to…not do that *)
  let stdin = Eio.Stdenv.stdin env |> Eio.Buf_read.of_flow ~max_size:Int.max_int
  and stdout = Eio.Stdenv.stdout env in
  let read = read_with_prompt stdin stdout in
  let env = Eval.Env.of_eio_env env in
  let rec loop env checkpoint =
    let prompt = match checkpoint with Some _ -> "… " | None -> "> " in
    let* input = read prompt in
    match input with
    | Some line -> (
        match eval_string ?continue_from:checkpoint env (line ^ "\n") with
        | Ok (env, result) ->
            repl_print stdout result;
            loop env None
        | Error (`Needs_input continue) -> loop env (Some continue)
        | Error err ->
            Eio.Flow.copy_string (error_to_string err ^ "\n") stdout;
            loop env None)
    | None -> Ok 0
  in
  loop env None |> Result.map_err error_to_string
