open! ContainersLabels
open Fun.Infix

let ast_of_string = Sedlexing.Utf8.from_string %> Ast.parse

let assert_parses str =
  let buf = Sedlexing.Utf8.from_string str in
  match Ast.parse buf with
  | Ok _ -> assert true
  | Error (`Syntax msg) -> failwith msg
  | _ -> assert false

let%test_unit "some operators" = assert_parses "2 + 3 == 4 / 5, !6"
