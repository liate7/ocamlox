let eval str =
  Eio_main.run @@ fun env ->
  let stream = Eio.Flow.string_source str in
  Ocamlox.eval_file stream env

let test str = match eval str with Ok _ -> () | Error str -> print_endline str
