open Lib

let%expect_test "after_else.lox" =
  test
    {|
    fun f() {
      if (false) "no"; else return "ok";
    }
    
    print f(); // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "after_if.lox" =
  test
    {|
    fun f() {
      if (true) return "ok";
    }
    
    print f(); // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "after_while.lox" =
  test
    {|
    fun f() {
      while (true) return "ok";
    }
    
    print f(); // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "at_top_level.lox" =
  test
    {|
    return "wat"; // Error at 'return': Can't return from top-level code.
    |};
  [%expect {| Evaluation error: can't return outside function |}]

let%expect_test "in_function.lox" =
  test
    {|
    fun f() {
      return "ok";
      print "bad";
    }
    
    print f(); // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "in_method.lox" =
  test
    {|
    class Foo {
      method() {
        return "ok";
        print "bad";
      }
    }
    
    print Foo().method(); // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "return_nil_if_no_value.lox" =
  test
    {|
    fun f() {
      return;
      print "bad";
    }
    
    print f(); // expect: nil
    |};
  [%expect {| nil |}]
