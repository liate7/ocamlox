open Lib

let%expect_test "body_must_be_block.lox" =
  test {|
    fun f() 123;
    print "ok";
    |};
  [%expect {| ok |}]

let%expect_test "empty_body.lox" =
  test {|
    fun f() {}
    print f(); // expect: nil
    |};
  [%expect {| nil |}]

let%expect_test "extra_arguments.lox" =
  test
    {|
    fun f(a, b) {
      print a;
      print b;
    }
    
    f(1, 2, 3, 4); // expect runtime error: Expected 2 arguments but got 4.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 4 |}]

let%expect_test "local_mutual_recursion.lox" =
  test
    {|
    {
      fun isEven(n) {
        if (n == 0) return true;
        return isOdd(n - 1); // expect runtime error: Undefined variable 'isOdd'.
      }
    
      fun isOdd(n) {
        if (n == 0) return false;
        return isEven(n - 1);
      }
    
      isEven(4);
    }
    |};
  [%expect {| Evaluation error: tried to reference unbound variable isOdd |}]

let%expect_test "local_recursion.lox" =
  test
    {|
    {
      fun fib(n) {
        if (n < 2) return n;
        return fib(n - 1) + fib(n - 2);
      }
    
      print fib(8); // expect: 21
    }
    |};
  [%expect {| 21 |}]

let%expect_test "missing_arguments.lox" =
  test
    {|
    fun f(a, b) {}

    f(1); // expect runtime error: Expected 2 arguments but got 1.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 1 |}]

let%expect_test "missing_comma_in_parameters.lox" =
  test
    {|
    // [line 3] Error at 'c': Expect ')' after parameters.
    // [c line 4] Error at end: Expect '}' after block.
    fun foo(a, b c, d, e, f) {}
    |};
  [%expect {| Syntax error at line 6, column 17 (c) (last token: ID(c)) |}]

let%expect_test "mutual_recursion.lox" =
  test
    {|
    fun isEven(n) {
      if (n == 0) return true;
      return isOdd(n - 1);
    }
    
    fun isOdd(n) {
      if (n == 0) return false;
      return isEven(n - 1);
    }
    
    print isEven(4); // expect: true
    print isOdd(3); // expect: true
    |};
  [%expect {|
    true
    true |}]

let%expect_test "nested_call_with_arguments.lox" =
  test
    {|
    fun returnArg(arg) {
      return arg;
    }
    
    fun returnFunCallWithArg(func, arg) {
      return returnArg(func)(arg);
    }
    
    fun printArg(arg) {
      print arg;
    }
    
    returnFunCallWithArg(printArg, "hello world"); // expect: hello world
    |};
  [%expect {| hello world |}]

let%expect_test "parameters.lox" =
  test
    {|
    fun f0() { return 0; }
    print f0(); // expect: 0
    
    fun f1(a) { return a; }
    print f1(1); // expect: 1
    
    fun f2(a, b) { return a + b; }
    print f2(1, 2); // expect: 3
    
    fun f3(a, b, c) { return a + b + c; }
    print f3(1, 2, 3); // expect: 6
    
    fun f4(a, b, c, d) { return a + b + c + d; }
    print f4(1, 2, 3, 4); // expect: 10
    
    fun f5(a, b, c, d, e) { return a + b + c + d + e; }
    print f5(1, 2, 3, 4, 5); // expect: 15
    
    fun f6(a, b, c, d, e, f) { return a + b + c + d + e + f; }
    print f6(1, 2, 3, 4, 5, 6); // expect: 21
    
    fun f7(a, b, c, d, e, f, g) { return a + b + c + d + e + f + g; }
    print f7(1, 2, 3, 4, 5, 6, 7); // expect: 28
    
    fun f8(a, b, c, d, e, f, g, h) { return a + b + c + d + e + f + g + h; }
    print f8(1, 2, 3, 4, 5, 6, 7, 8); // expect: 36
    |};
  [%expect {|
    0
    1
    3
    6
    10
    15
    21
    28
    36 |}]

let%expect_test "print.lox" =
  test
    {|
    fun foo() {}
    print foo; // expect: <fn foo>
    
    print clock; // expect: <native fn>
    |};
  [%expect {|
    #<function foo()>
    #<builtin clock()> |}]

let%expect_test "recursion.lox" =
  test
    {|
    fun fib(n) {
      if (n < 2) return n;
      return fib(n - 1) + fib(n - 2);
    }
    
    print fib(8); // expect: 21
    |};
  [%expect {| 21 |}]
