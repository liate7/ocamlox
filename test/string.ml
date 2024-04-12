open Lib

let%expect_test "error_after_multiline.lox" =
  test
    {|
    // Tests that we correctly track the line info across multiline strings.
    var a = "1
    2
    3
    ";
    
    err; // // expect runtime error: Undefined variable 'err'.
    |};
  [%expect {| Evaluation error: tried to reference unbound variable err |}]

let%expect_test "literals.lox" =
  test
    {|
    print "(" + "" + ")";   // expect: ()
    print "a string"; // expect: a string
    
    // Non-ASCII.
    print "A~¶Þॐஃ"; // expect: A~¶Þॐஃ
    |};
  [%expect {|
    ()
    a string
    A~¶Þॐஃ |}]

let%expect_test "multiline.lox" =
  test {|
var a = "1
2
3";
print a;
// expect: 1
// expect: 2
// expect: 3
|};
  [%expect {|
    1
    2
    3 |}]

let%expect_test "unterminated.lox" =
  test
    {|
    // [line 2] Error: Unterminated string.
    "this string has no close quote
    |};
  [%expect {| Parsing error: Unterminated string |}]
