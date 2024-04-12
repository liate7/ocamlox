open! ContainersLabels
open! Eio.Std

let handle_case ~dest ~source name =
  let input = Eio.Buf_read.(parse_exn take_all) ~max_size:Int.max_int source in
  let helper = "test" in
  dest
  |> Eio.Flow.copy_string
       [%string
         {foo|
let%expect_test "%{name}" =
  %{helper} {|
%{input}|};
 [%expect {||}]

         |foo}]

let handle_dir ~dest ~source =
  let files = Eio.Path.read_dir source in
  dest |> Eio.Flow.copy_string "open Lib\n\n";
  List.iter files ~f:(fun file ->
      Eio.Path.(with_open_in (source / file)) @@ fun source ->
      handle_case ~dest ~source @@ Filename.basename file)

let copy_tests ~from ~into =
  Eio.Path.read_dir from
  |> List.iter ~f:(fun name ->
         match Filename.extension name with
         | "" ->
             (* is a dir *)
             Eio.Path.with_open_dir
               Eio.Path.(from / name)
               (fun source ->
                 try
                   Eio.Path.with_open_out ~create:(`Exclusive 0o640)
                     Eio.Path.(into / (name ^ ".ml"))
                   @@ fun dest -> handle_dir ~dest ~source
                 with Eio.Io _ ->
                   traceln "%s already exists, ignoring"
                     Eio.Path.(into / (name ^ ".ml") |> native_exn))
         | _ -> ())

let run from into =
  Eio_main.run @@ fun env ->
  Eio.Path.with_open_dir Eio.Path.(Eio.Stdenv.fs env / from) @@ fun from ->
  Eio.Path.with_open_dir Eio.Path.(Eio.Stdenv.fs env / into) @@ fun into ->
  copy_tests ~from ~into

let () =
  let open Cmdliner in
  let from =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"SOURCE" ~doc:"The tree of test files")
  and into =
    Arg.(
      required
      & pos 1 (some file) None
      & info [] ~docv:"DEST" ~doc:"The directory to put the expect tests")
  in
  let term = Term.(const run $ from $ into)
  and info =
    Cmd.info ~doc:"Generate expect tests from the craftinginterpreters tests"
      "generate"
  in
  Cmd.v info term |> Cmd.eval |> exit
