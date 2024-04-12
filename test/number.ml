open Lib

let%expect_test "decimal_point_at_eof.lox" =
  test
    {|
    // [line 2] Error at end: Expect property name after '.'.
    123.
    |};
  [%expect {| Parsing error: unexpected eof |}]

let%expect_test "leading_dot.lox" =
  test {|
    // [line 2] Error at '.': Expect expression.
    .123;
    |};
  [%expect {| Syntax error at line 4, column 4 (.) (last token: .) |}]

let%expect_test "literals.lox" =
  test
    {|
    print 123;     // expect: 123
    print 987654;  // expect: 987654
    print 0;       // expect: 0
    print -0;      // expect: -0

    print 123.456; // expect: 123.456
    print -0.001;  // expect: -0.001
    |};
  [%expect {|
    123
    987654
    0
    0
    123.456
    -0.001 |}]

let%expect_test "trailing_dot.lox" =
  test
    {|
    // [line 2] Error at ';': Expect property name after '.'.
    123.;
    |};
  [%expect {| Syntax error at line 4, column 8 (;) (last token: ;) |}]
