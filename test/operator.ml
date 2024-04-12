open Lib

let%expect_test "add.lox" =
  test
    {|
    print 123 + 456; // expect: 579
    print "str" + "ing"; // expect: string
    |};
  [%expect {|
    579
    string |}]

let%expect_test "add_bool_nil.lox" =
  test
    {|
    true + nil; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add true with nil |}]

let%expect_test "add_bool_num.lox" =
  test
    {|
    true + 123; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add true with 123 |}]

let%expect_test "add_bool_string.lox" =
  test
    {|
    true + "s"; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add true with "s" |}]

let%expect_test "add_nil_nil.lox" =
  test
    {|
    nil + nil; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add nil with nil |}]

let%expect_test "add_num_nil.lox" =
  test
    {|
    1 + nil; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add 1 with nil |}]

let%expect_test "add_string_nil.lox" =
  test
    {|
    "s" + nil; // expect runtime error: Operands must be two numbers or two strings.
    |};
  [%expect {| Evaluation error: can't add "s" with nil |}]

let%expect_test "comparison.lox" =
  test
    {|
    print 1 < 2;    // expect: true
    print 2 < 2;    // expect: false
    print 2 < 1;    // expect: false
    
    print 1 <= 2;    // expect: true
    print 2 <= 2;    // expect: true
    print 2 <= 1;    // expect: false
    
    print 1 > 2;    // expect: false
    print 2 > 2;    // expect: false
    print 2 > 1;    // expect: true
    
    print 1 >= 2;    // expect: false
    print 2 >= 2;    // expect: true
    print 2 >= 1;    // expect: true
    
    // Zero and negative zero compare the same.
    print 0 < -0; // expect: false
    print -0 < 0; // expect: false
    print 0 > -0; // expect: false
    print -0 > 0; // expect: false
    print 0 <= -0; // expect: true
    print -0 <= 0; // expect: true
    print 0 >= -0; // expect: true
    print -0 >= 0; // expect: true
    |};
  [%expect
    {|
    true
    false
    false
    true
    true
    false
    false
    false
    true
    false
    true
    true
    false
    false
    false
    false
    true
    true
    true
    true |}]

let%expect_test "divide.lox" =
  test
    {|
    print 8 / 2;         // expect: 4
    print 12.34 / 12.34;  // expect: 1
    |};
  [%expect {|
    4
    1 |}]

let%expect_test "divide_nonnum_num.lox" =
  test {|
    "1" / 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "divide_num_nonnum.lox" =
  test {|
    1 / "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "equals.lox" =
  test
    {|
    print nil == nil; // expect: true

    print true == true; // expect: true
    print true == false; // expect: false
    
    print 1 == 1; // expect: true
    print 1 == 2; // expect: false
    
    print "str" == "str"; // expect: true
    print "str" == "ing"; // expect: false
    
    print nil == false; // expect: false
    print false == 0; // expect: false
    print 0 == "0"; // expect: false
    |};
  [%expect
    {|
    true
    true
    false
    true
    false
    true
    false
    false
    false
    false |}]

let%expect_test "equals_class.lox" =
  test
    {|
    // Bound methods have identity equality.
    class Foo {}
    class Bar {}
    
    print Foo == Foo; // expect: true
    print Foo == Bar; // expect: false
    print Bar == Foo; // expect: false
    print Bar == Bar; // expect: true
    
    print Foo == "Foo"; // expect: false
    print Foo == nil;   // expect: false
    print Foo == 123;   // expect: false
    print Foo == true;  // expect: false
    |};
  [%expect
    {|
    true
    false
    false
    true
    false
    false
    false
    false |}]

let%expect_test "equals_method.lox" =
  test
    {|
    // Bound methods have identity equality.
    class Foo {
      method() {}
    }
    
    var foo = Foo();
    var fooMethod = foo.method;
    
    // Same bound method.
    print fooMethod == fooMethod; // expect: true
    
    // Different closurizations.
    print foo.method == foo.method; // expect: false
    |};
  [%expect {|
    true
    false |}]

let%expect_test "greater_nonnum_num.lox" =
  test {|
    "1" > 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "greater_num_nonnum.lox" =
  test {|
    1 > "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "greater_or_equal_nonnum_num.lox" =
  test
    {|
    "1" >= 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "greater_or_equal_num_nonnum.lox" =
  test
    {|
    1 >= "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "less_nonnum_num.lox" =
  test {|
    "1" < 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "less_num_nonnum.lox" =
  test {|
    1 < "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "less_or_equal_nonnum_num.lox" =
  test
    {|
    "1" <= 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "less_or_equal_num_nonnum.lox" =
  test
    {|
    1 <= "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "multiply.lox" =
  test
    {|
    print 5 * 3; // expect: 15
    print 12.34 * 0.3; // expect: 3.702
    |};
  [%expect {|
    15
    3.702 |}]

let%expect_test "multiply_nonnum_num.lox" =
  test {|
    "1" * 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "multiply_num_nonnum.lox" =
  test {|
    1 * "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "negate.lox" =
  test
    {|
    print -(3); // expect: -3
    print --(3); // expect: 3
    print ---(3); // expect: -3
    |};
  [%expect {|
    -3
    3
    -3 |}]

let%expect_test "negate_nonnum.lox" =
  test {|
    -"s"; // expect runtime error: Operand must be a number.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "not.lox" =
  test
    {|
    print !true;     // expect: false
    print !false;    // expect: true
    print !!true;    // expect: true
    
    print !123;      // expect: false
    print !0;        // expect: false
    
    print !nil;     // expect: true
    
    print !"";       // expect: false
    
    fun foo() {}
    print !foo;      // expect: false
    |};
  [%expect
    {|
    false
    true
    true
    false
    false
    true
    false
    false |}]

let%expect_test "not_class.lox" =
  test
    {|
    class Bar {}
    print !Bar;      // expect: false
    print !Bar();    // expect: false
    |};
  [%expect {|
    false
    false |}]

let%expect_test "not_equals.lox" =
  test
    {|
    print nil != nil; // expect: false

    print true != true; // expect: false
    print true != false; // expect: true
    
    print 1 != 1; // expect: false
    print 1 != 2; // expect: true
    
    print "str" != "str"; // expect: false
    print "str" != "ing"; // expect: true
    
    print nil != false; // expect: true
    print false != 0; // expect: true
    print 0 != "0"; // expect: true
    |};
  [%expect
    {|
    false
    false
    true
    false
    true
    false
    true
    true
    true
    true |}]

let%expect_test "subtract.lox" =
  test {|
    print 4 - 3; // expect: 1
    print 1.2 - 1.2; // expect: 0
    |};
  [%expect {|
    1
    0 |}]

let%expect_test "subtract_nonnum_num.lox" =
  test {|
    "1" - 1; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]

let%expect_test "subtract_num_nonnum.lox" =
  test {|
    1 - "1"; // expect runtime error: Operands must be numbers.
    |};
  [%expect {| Evaluation error: Not a number |}]
