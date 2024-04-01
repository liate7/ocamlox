open! ContainersLabels
open! Eio.Std

let run file =
  Eio_main.run
  @@
  match file with None -> Ocamlox.repl | Some file -> Ocamlox.eval_file file

let () =
  let open Cmdliner in
  let file =
    Arg.(
      value
      & pos 1 (Arg.some file) None
      & info [] ~docv:"SCRIPT" ~doc:"The file to eval")
  in
  let term = Term.(const run $ file)
  and info =
    Cmd.info ~doc:"define a set of words with DuckDuckGo's API" "ddg"
  in
  Cmd.v info term |> Cmd.eval_result' |> exit
