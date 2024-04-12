open Lib

let%expect_test "class_in_else.lox" =
  test
    {|
    // [line 2] Error at 'class': Expect expression.
    if (true) "ok"; else class Foo {}
    |};
  [%expect {| Syntax error at line 4, column 25 (class) (last token: class) |}]

let%expect_test "class_in_then.lox" =
  test
    {|
    // [line 2] Error at 'class': Expect expression.
    if (true) class Foo {}
    |};
  [%expect {| Syntax error at line 4, column 14 (class) (last token: class) |}]

let%expect_test "dangling_else.lox" =
  test
    {|
    // A dangling else binds to the right-most if.
    if (true) if (false) print "bad"; else print "good"; // expect: good
    if (false) if (true) print "bad"; else print "bad";
    |};
  [%expect {| good |}]

let%expect_test "else.lox" =
  test
    {|
    // Evaluate the 'else' expression if the condition is false.
    if (true) print "good"; else print "bad"; // expect: good
    if (false) print "bad"; else print "good"; // expect: good
    
    // Allow block body.
    if (false) nil; else { print "block"; } // expect: block
    |};
  [%expect {|
    good
    good
    block |}]

let%expect_test "fun_in_else.lox" =
  test
    {|
    // [line 2] Error at 'fun': Expect expression.
    if (true) "ok"; else fun foo() {}
    |};
  [%expect {| Syntax error at line 4, column 25 (fun) (last token: defn) |}]

let%expect_test "fun_in_then.lox" =
  test
    {|
    // [line 2] Error at 'fun': Expect expression.
    if (true) fun foo() {}
    |};
  [%expect {| Syntax error at line 4, column 14 (fun) (last token: defn) |}]

let%expect_test "if.lox" =
  test
    {|
    // Evaluate the 'then' expression if the condition is true.
    if (true) print "good"; // expect: good
    if (false) print "bad";
    
    // Allow block body.
    if (true) { print "block"; } // expect: block
    
    // Assignment in if condition.
    var a = true;
    if (a = false) print a; // expect: false
    |};
  [%expect {|
    good
    block
    false |}]

let%expect_test "truth.lox" =
  test
    {|
    // False and nil are false.
    if (false) print "bad"; else print "false"; // expect: false
    if (nil) print "bad"; else print "nil"; // expect: nil
    
    // Everything else is true.
    if (true) print true; // expect: true
    if (0) print 0; // expect: 0
    if ("") print "empty"; // expect: empty
    |};
  [%expect {|
    false
    nil
    true
    0
    empty |}]

let%expect_test "var_in_else.lox" =
  test
    {|
    // [line 2] Error at 'var': Expect expression.
    if (true) "ok"; else var foo = nil;
    |};
  [%expect {| Syntax error at line 4, column 25 (var) (last token: let) |}]

let%expect_test "var_in_then.lox" =
  test
    {|
    // [line 2] Error at 'var': Expect expression.
    if (true) var foo = nil;
    |};
  [%expect {| Syntax error at line 4, column 14 (var) (last token: let) |}]
