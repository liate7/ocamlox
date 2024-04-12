open Lib

let%expect_test "empty" =
  test {|
    class Foo {}

    print Foo;
    |};
  [%expect {| #<class Foo> |}]

let%expect_test "inherit_self" =
  test {|
    class Foo < Foo {}
    |};
  [%expect {| Evaluation error: a class can't inherit from itself. |}]

let%expect_test "inherited_method" =
  test
    {|
    class Foo {
      inFoo() {
        print "in foo";
      }
    }
    
    class Bar < Foo {
      inBar() {
        print "in bar";
      }
    }
    
    class Baz < Bar {
      inBaz() {
        print "in baz";
      }
    }
    
    var baz = Baz();
    baz.inFoo(); // expect: in foo
    baz.inBar(); // expect: in bar
    baz.inBaz(); // expect: in baz
    |};
  [%expect {|
    in foo
    in bar
    in baz |}]

let%expect_test "local_inherit_other" =
  test
    {|
    class A {}
    
    fun f() {
      class B < A {}
      return B;
    }
    
    print f(); // expect: B
    |};
  [%expect {| #<class B> |}]

let%expect_test "local_inherit_self" =
  test
    {|
    {
      class Foo < Foo {} // Error at 'Foo': A class can't inherit from itself.
    }
    |};
  [%expect {| Evaluation error: a class can't inherit from itself. |}]

let%expect_test "local_reference_self" =
  test
    {|
    {
      class Foo {
        returnSelf() {
          return Foo;
        }
      }
    
      print Foo().returnSelf(); // expect: Foo
    }
    |};
  [%expect {| #<class Foo> |}]

let%expect_test "reference_self" =
  test
    {|
    class Foo {
      returnSelf() {
        return Foo;
      }
    }
    
    print Foo().returnSelf(); // expect: Foo
    |};
  [%expect {| #<class Foo> |}]
