open Lib

let%expect_test "arguments.lox" =
  test
    {|
    class Foo {
      init(a, b) {
        print "init"; // expect: init
        this.a = a;
        this.b = b;
      }
    }
    
    var foo = Foo(1, 2);
    print foo.a; // expect: 1
    print foo.b; // expect: 2
    |};
  [%expect {|
    init
    1
    2 |}]

let%expect_test "call_init_early_return.lox" =
  test
    {|
    class Foo {
      init() {
        print "init";
        return;
        print "nope";
      }
    }
    
    var foo = Foo(); // expect: init
    print foo.init(); // expect: init
    // expect: Foo instance
   |};
  [%expect {|
    init
    init
    #<class Foo instance> |}]

let%expect_test "call_init_explicitly.lox" =
  test
    {|
    class Foo {
      init(arg) {
        print "Foo.init(" + arg + ")";
        this.field = "init";
      }
    }
    
    var foo = Foo("one"); // expect: Foo.init(one)
    foo.field = "field";
    
    var foo2 = foo.init("two"); // expect: Foo.init(two)
    print foo2; // expect: Foo instance
    
    // Make sure init() doesn't create a fresh instance.
    print foo.field; // expect: init
    |};
  [%expect {|
    Foo.init(one)
    Foo.init(two)
    #<class Foo instance>
    init |}]

let%expect_test "default.lox" =
  test
    {|
    class Foo {}

    var foo = Foo();
    print foo; // expect: Foo instance
    |};
  [%expect {| #<class Foo instance> |}]

let%expect_test "default_arguments.lox" =
  test
    {|
    class Foo {}

    var foo = Foo(1, 2, 3); // expect runtime error: Expected 0 arguments but got 3.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 0 args, got 3 |}]

let%expect_test "early_return.lox" =
  test
    {|
    class Foo {
      init() {
        print "init";
        return;
        print "nope";
      }
    }
    
    var foo = Foo(); // expect: init
    print foo; // expect: Foo instance
    |};
  [%expect {|
    init
    #<class Foo instance> |}]

let%expect_test "extra_arguments.lox" =
  test
    {|
    class Foo {
      init(a, b) {
        this.a = a;
        this.b = b;
      }
    }
    
    var foo = Foo(1, 2, 3, 4); // expect runtime error: Expected 2 arguments but got 4.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 4 |}]

let%expect_test "init_not_method.lox" =
  test
    {|
    class Foo {
      init(arg) {
        print "Foo.init(" + arg + ")";
        this.field = "init";
      }
    }
    
    fun init() {
      print "not initializer";
    }
    
    init(); // expect: not initializer
    |};
  [%expect {| not initializer |}]

let%expect_test "missing_arguments.lox" =
  test
    {|
    class Foo {
      init(a, b) {}
    }
    
    var foo = Foo(1); // expect runtime error: Expected 2 arguments but got 1.
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 1 |}]

let%expect_test "return_in_nested_function.lox" =
  test
    {|
    class Foo {
      init() {
        fun init() {
          return "bar";
        }
        print init(); // expect: bar
      }
    }
    
    print Foo(); // expect: Foo instance
    |};
  [%expect {|
    bar
    #<class Foo instance> |}]

let%expect_test "return_value.lox" =
  test
    {|
    class Foo {
      init() {
        return "result"; // Error at 'return': Can't return a value from an initializer.
      }
    }
    |};
  [%expect {| Evaluation error: can't return a value from an initializer |}]
