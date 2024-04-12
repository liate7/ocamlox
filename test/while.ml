open Lib

let%expect_test "class_in_body.lox" =
  test
    {|
    // [line 2] Error at 'class': Expect expression.
    while (true) class Foo {}
    |};
  [%expect {| Syntax error at line 4, column 17 (class) (last token: class) |}]

let%expect_test "closure_in_body.lox" =
  test
    {|
    var f1 = nil;
    var f2 = nil;
    var f3 = nil;
    
    var i = 1;
    while (i < 4) {
      var j = i;
      fun f() { print j; }
    
      if (j == 1) f1 = f;
      else if (j == 2) f2 = f;
      else f3 = f;
    
      i = i + 1;
    }
    
    f1(); // expect: 1
    f2(); // expect: 2
    f3(); // expect: 3
    |};
  [%expect {|
    1
    2
    3 |}]

let%expect_test "fun_in_body.lox" =
  test
    {|
    // [line 2] Error at 'fun': Expect expression.
    while (true) fun foo() {}
    |};
  [%expect {| Syntax error at line 4, column 17 (fun) (last token: defn) |}]

let%expect_test "return_closure.lox" =
  test
    {|
    fun f() {
      while (true) {
        var i = "i";
        fun g() { print i; }
        return g;
      }
    }
    
    var h = f();
    h(); // expect: i
    |};
  [%expect {| i |}]

let%expect_test "return_inside.lox" =
  test
    {|
    fun f() {
      var i = 0;
      while (i < 100) {
        return i;
        i = i + 1;
      }
    }
    
    print f();
    |};
  [%expect {| 0 |}]

let%expect_test "syntax.lox" =
  test
    {|
    // Single-expression body.
    var c = 0;
    while (c < 3) print c = c + 1;
    // expect: 0
    // expect: 1
    // expect: 2
    
    // Block body.
    var a = 0;
    while (a < 3) {
      print a;
      a = a + 1;
    }
    // expect: 0
    // expect: 1
    // expect: 2
    
    // Statement bodies.
    while (false) if (true) 1; else 2;
    while (false) while (true) 1;
    |};
  [%expect {|
    0
    1
    2
    0
    1
    2 |}]

let%expect_test "var_in_body.lox" =
  test
    {|
    // [line 2] Error at 'var': Expect expression.
    while (true) var foo = nil;
    |};
  [%expect {| Syntax error at line 4, column 17 (var) (last token: let) |}]
