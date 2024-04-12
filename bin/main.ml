open! ContainersLabels
open! Eio.Std

let run file =
  Eio_main.run @@ fun env ->
  match file with
  | None -> Ocamlox.repl env
  | Some file ->
      let file =
        if Filename.is_relative file then Eio.Path.(Eio.Stdenv.cwd env / file)
        else Eio.Path.(Eio.Stdenv.fs env / file)
      in
      Eio.Path.with_open_in file @@ fun file -> Ocamlox.eval_file file env

let () =
  let open Cmdliner in
  let file =
    Arg.(
      value
      & pos 0 (Arg.some file) None
      & info [] ~docv:"SCRIPT" ~doc:"The file to eval")
  in
  let term = Term.(const run $ file)
  and info = Cmd.info ~doc:"run an ocamlox program / repl" "ocamlox" in
  Cmd.v info term |> Cmd.eval_result' |> exit
