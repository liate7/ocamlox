open Lib

let%expect_test "closure.lox" =
  test
    {|
    class Foo {
      getClosure() {
        fun closure() {
          return this.toString();
        }
        return closure;
      }
    
      toString() { return "Foo"; }
    }
    
    var closure = Foo().getClosure();
    print closure(); // expect: Foo
    |};
  [%expect {| Foo |}]

let%expect_test "nested_class.lox" =
  test
    {|
    class Outer {
      method() {
        print this; // expect: Outer instance
    
        fun f() {
          print this; // expect: Outer instance
    
          class Inner {
            method() {
              print this; // expect: Inner instance
            }
          }
    
          Inner().method();
        }
        f();
      }
    }
    
    Outer().method();
    |};
  [%expect {|
    #<class Outer instance>
    #<class Outer instance>
    #<class Inner instance> |}]

let%expect_test "nested_closure.lox" =
  test
    {|
    class Foo {
      getClosure() {
        fun f() {
          fun g() {
            fun h() {
              return this.toString();
            }
            return h;
          }
          return g;
        }
        return f;
      }
    
      toString() { return "Foo"; }
    }
    
    var closure = Foo().getClosure();
    print closure()()(); // expect: Foo
    |};
  [%expect {| Foo |}]

let%expect_test "this_at_top_level.lox" =
  test
    {|
    this; // Error at 'this': Can't use 'this' outside of a class.
    |};
  [%expect {| Evaluation error: can't reference "this" outside a class |}]

let%expect_test "this_in_method.lox" =
  test
    {|
    class Foo {
      bar() { return this; }
      baz() { return "baz"; }
    }
    
    print Foo().bar().baz(); // expect: baz
    |};
  [%expect {| baz |}]

let%expect_test "this_in_top_level_function.lox" =
  test
    {|
    fun foo() {
      this; // Error at 'this': Can't use 'this' outside of a class.
    }
    |};
  [%expect {| Evaluation error: can't reference "this" outside a class |}]
