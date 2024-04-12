(* Most of these are copied from
   [[https://github.com/munificent/craftinginterpreters/tree/master/test]] *)

open Lib

let%expect_test "associativity" =
  test
    {|
    var a = "a";
    var b = "b";
    var c = "c";

    a = b = c;
    print a;
    print b;
    print c;
    |};
  [%expect {|
    b
    c
    c |}]

let%expect_test "global" =
  test
    {|
    var a = "before";
    print a;

    a = "after";
    print a;
    
    print a = "arg";
    print a;
    |};
  [%expect {|
    before
    after
    after
    arg
    |}]

let%expect_test "grouping" =
  test {|
    var a = "a";
    (a) = "value";
    |};
  [%expect {| Syntax error at line 3, column 8 (=) (last token: =) |}]

let%expect_test "infix_operator" =
  test {|
    var a = "a";
    var b = "b";
    a + b = "value";
    |};
  [%expect {| Syntax error at line 4, column 10 (=) (last token: =) |}]

let%expect_test "local" =
  test
    {|
    {
      var a = "before";
      print a; // expect: before
    
      a = "after";
      print a; // expect: after
    
      print a = "arg"; // expect: arg
      print a; // expect: arg
    }
    |};
  [%expect {|
    before
    after
    after
    arg |}]

let%expect_test "prefix_operator" =
  test {|
    var a = "a";
    !a = "value";
    |};
  [%expect {| Syntax error at line 3, column 7 (=) (last token: =) |}]

let%expect_test "syntax" =
  test
    {|
    var a = "before";
    var c = a = "var";
    print a;
    print c;
    |};
  [%expect {|
    var
    before |}]

let%expect_test "to_this" =
  test
    {|
    class Foo {
      Foo() {
        this = "value"; // Error at '=': Invalid assignment target.
      }
    }

    Foo();
    |};
  [%expect {| Syntax error at line 4, column 13 (=) (last token: =) |}]

let%expect_test "undefined" =
  test {|
    unknown = "what";
    |};
  [%expect {| Evaluation error: tried to set unbound variable unknown |}]
