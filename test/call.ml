open Lib

let%expect_test "bool" =
  test {|
    true();
    |};
  [%expect
    {| Evaluation error: can't call this; can only call functions and classes |}]

let%expect_test "nil" =
  test {|
    nil();
    |};
  [%expect
    {| Evaluation error: can't call this; can only call functions and classes |}]

let%expect_test "nil" =
  test {|
    123();
    |};
  [%expect
    {| Evaluation error: can't call this; can only call functions and classes |}]

let%expect_test "nil" =
  test {|
    class Foo {}

    var foo = Foo();
    foo();
    |};
  [%expect
    {| Evaluation error: can't call this; can only call functions and classes |}]

let%expect_test "nil" =
  test {|
    "str"();
    |};
  [%expect
    {| Evaluation error: can't call this; can only call functions and classes |}]
