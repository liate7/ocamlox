open Lib

let%expect_test "missing_argument.lox" =
  test {|
    // [line 2] Error at ';': Expect expression.
    print;
    |};
  [%expect {| Syntax error at line 4, column 9 (;) (last token: ;) |}]
