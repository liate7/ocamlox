open Lib

let%expect_test "arity.lox" =
  test
    {|
    class Foo {
      method0() { return "no args"; }
      method1(a) { return a; }
      method2(a, b) { return a + b; }
      method3(a, b, c) { return a + b + c; }
      method4(a, b, c, d) { return a + b + c + d; }
      method5(a, b, c, d, e) { return a + b + c + d + e; }
      method6(a, b, c, d, e, f) { return a + b + c + d + e + f; }
      method7(a, b, c, d, e, f, g) { return a + b + c + d + e + f + g; }
      method8(a, b, c, d, e, f, g, h) { return a + b + c + d + e + f + g + h; }
    }
    
    var foo = Foo();
    print foo.method0(); // expect: no args
    print foo.method1(1); // expect: 1
    print foo.method2(1, 2); // expect: 3
    print foo.method3(1, 2, 3); // expect: 6
    print foo.method4(1, 2, 3, 4); // expect: 10
    print foo.method5(1, 2, 3, 4, 5); // expect: 15
    print foo.method6(1, 2, 3, 4, 5, 6); // expect: 21
    print foo.method7(1, 2, 3, 4, 5, 6, 7); // expect: 28
    print foo.method8(1, 2, 3, 4, 5, 6, 7, 8); // expect: 36
    |};
  [%expect
    {|
    no args
    1
    3
    6
    10
    15
    21
    28
    36 |}]

let%expect_test "empty_block.lox" =
  test
    {|
    class Foo {
      bar() {}
    }
    
    print Foo().bar(); // expect: nil
    |};
  [%expect {| nil |}]

let%expect_test "extra_arguments.lox" =
  test
    {|
    class Foo {
      method(a, b) {
        print a;
        print b;
      }
    }
    
    Foo().method(1, 2, 3, 4); // expect runtime error: Expected 2 arguments but got 4.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 4 |}]

let%expect_test "missing_arguments.lox" =
  test
    {|
    class Foo {
      method(a, b) {}
    }
    
    Foo().method(1); // expect runtime error: Expected 2 arguments but got 1.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 1 |}]

let%expect_test "not_found.lox" =
  test
    {|
    class Foo {}

    Foo().unknown(); // expect runtime error: Undefined property 'unknown'.
    |};
  [%expect {| Evaluation error: undefined attribute unknown |}]

let%expect_test "print_bound_method.lox" =
  test
    {|
    class Foo {
      method() { }
    }
    var foo = Foo();
    print foo.method; // expect: <fn method>
    |};
  [%expect {| #<function method()> |}]

let%expect_test "refer_to_name.lox" =
  test
    {|
    class Foo {
      method() {
        print method; // expect runtime error: Undefined variable 'method'.
      }
    }
    
    Foo().method();
    |};
  [%expect {| Evaluation error: tried to reference unbound variable method |}]
