open Lib

let%expect_test "constructor.lox" =
  test
    {|
    class A {
      init(param) {
        this.field = param;
      }
    
      test() {
        print this.field;
      }
    }
    
    class B < A {}
    
    var b = B("value");
    b.test(); // expect: value
    |};
  [%expect {| value |}]

let%expect_test "inherit_from_function.lox" =
  test
    {|
    fun foo() {}

    class Subclass < foo {} // expect runtime error: Superclass must be a class.
    |};
  [%expect {| Evaluation error: superclass must be a class |}]

let%expect_test "inherit_from_nil.lox" =
  test
    {|
    var Nil = nil;
    class Foo < Nil {} // expect runtime error: Superclass must be a class.
    |};
  [%expect {| Evaluation error: superclass must be a class |}]

let%expect_test "inherit_from_number.lox" =
  test
    {|
    var Number = 123;
    class Foo < Number {} // expect runtime error: Superclass must be a class.
    |};
  [%expect {| Evaluation error: superclass must be a class |}]

let%expect_test "inherit_methods.lox" =
  test
    {|
    class Foo {
      methodOnFoo() { print "foo"; }
      override() { print "foo"; }
    }
    
    class Bar < Foo {
      methodOnBar() { print "bar"; }
      override() { print "bar"; }
    }
    
    var bar = Bar();
    bar.methodOnFoo(); // expect: foo
    bar.methodOnBar(); // expect: bar
    bar.override(); // expect: bar
    |};
  [%expect {|
    foo
    bar
    bar |}]

let%expect_test "parenthesized_superclass.lox" =
  test
    {|
    class Foo {}

    // [line 4] Error at '(': Expect superclass name.
    class Bar < (Foo) {}
    |};
  [%expect {| Syntax error at line 6, column 16 (() (last token: () |}]

let%expect_test "set_fields_from_base_class.lox" =
  test
    {|
    class Foo {
      foo(a, b) {
        this.field1 = a;
        this.field2 = b;
      }
    
      fooPrint() {
        print this.field1;
        print this.field2;
      }
    }
    
    class Bar < Foo {
      bar(a, b) {
        this.field1 = a;
        this.field2 = b;
      }
    
      barPrint() {
        print this.field1;
        print this.field2;
      }
    }
    
    var bar = Bar();
    bar.foo("foo 1", "foo 2");
    bar.fooPrint();
    // expect: foo 1
    // expect: foo 2
    
    bar.bar("bar 1", "bar 2");
    bar.barPrint();
    // expect: bar 1
    // expect: bar 2
    
    bar.fooPrint();
    // expect: bar 1
    // expect: bar 2
    |};
  [%expect {|
    foo 1
    foo 2
    bar 1
    bar 2
    bar 1
    bar 2 |}]
