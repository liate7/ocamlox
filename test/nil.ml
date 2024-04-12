open Lib

let%expect_test "literal.lox" =
  test {|
     print nil; // expect: nil
     |};
  [%expect {| nil |}]
