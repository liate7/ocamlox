open Lib

let%expect_test "bound_method.lox" =
  test
    {|
    class A {
      method(arg) {
        print "A.method(" + arg + ")";
      }
    }
    
    class B < A {
      getClosure() {
        return super.method;
      }
    
      method(arg) {
        print "B.method(" + arg + ")";
      }
    }
    
    
    var closure = B().getClosure();
    closure("arg"); // expect: A.method(arg)
    |};
  [%expect {| A.method(arg) |}]

let%expect_test "call_other_method.lox" =
  test
    {|
    class Base {
      foo() {
        print "Base.foo()";
      }
    }
    
    class Derived < Base {
      bar() {
        print "Derived.bar()";
        super.foo();
      }
    }
    
    Derived().bar();
    // expect: Derived.bar()
    // expect: Base.foo()
    |};
  [%expect {|
    Derived.bar()
    Base.foo() |}]

let%expect_test "call_same_method.lox" =
  test
    {|
    class Base {
      foo() {
        print "Base.foo()";
      }
    }
    
    class Derived < Base {
      foo() {
        print "Derived.foo()";
        super.foo();
      }
    }
    
    Derived().foo();
    // expect: Derived.foo()
    // expect: Base.foo()
    |};
  [%expect {|
    Derived.foo()
    Base.foo() |}]

let%expect_test "closure.lox" =
  test
    {|
    class Base {
      toString() { return "Base"; }
    }
    
    class Derived < Base {
      getClosure() {
        fun closure() {
          return super.toString();
        }
        return closure;
      }
    
      toString() { return "Derived"; }
    }
    
    var closure = Derived().getClosure();
    print closure(); // expect: Base
    |};
  [%expect {| Base |}]

let%expect_test "constructor.lox" =
  test
    {|
    class Base {
      init(a, b) {
        print "Base.init(" + a + ", " + b + ")";
      }
    }
    
    class Derived < Base {
      init() {
        print "Derived.init()";
        super.init("a", "b");
      }
    }
    
    Derived();
    // expect: Derived.init()
    // expect: Base.init(a, b)
    |};
  [%expect {|
    Derived.init()
    Base.init(a, b) |}]

let%expect_test "extra_arguments.lox" =
  test
    {|
    class Base {
      foo(a, b) {
        print "Base.foo(" + a + ", " + b + ")";
      }
    }
    
    class Derived < Base {
      foo() {
        print "Derived.foo()"; // expect: Derived.foo()
        super.foo("a", "b", "c", "d"); // expect runtime error: Expected 2 arguments but got 4.
      }
    }
    
    Derived().foo();
    |};
  [%expect
    {|
    Derived.foo()
    Evaluation error: arity mismatch: expected 2 args, got 4 |}]

let%expect_test "indirectly_inherited.lox" =
  test
    {|
    class A {
      foo() {
        print "A.foo()";
      }
    }
    
    class B < A {}
    
    class C < B {
      foo() {
        print "C.foo()";
        super.foo();
      }
    }
    
    C().foo();
    // expect: C.foo()
    // expect: A.foo()
    |};
  [%expect {|
    C.foo()
    A.foo() |}]

let%expect_test "missing_arguments.lox" =
  test
    {|
    class Base {
      foo(a, b) {
        print "Base.foo(" + a + ", " + b + ")";
      }
    }
    
    class Derived < Base {
      foo() {
        super.foo(1); // expect runtime error: Expected 2 arguments but got 1.
      }
    }
    
    Derived().foo();
    |};
  [%expect {| Evaluation error: arity mismatch: expected 2 args, got 1 |}]

let%expect_test "no_superclass_bind.lox" =
  test
    {|
    class Base {
      foo() {
        super.doesNotExist; // Error at 'super': Can't use 'super' in a class with no superclass.
      }
    }
    
    Base().foo();
    |};
  [%expect
    {| Evaluation error: can't reference "super" in a class with no superclass |}]

let%expect_test "no_superclass_call.lox" =
  test
    {|
    class Base {
      foo() {
        super.doesNotExist(1); // Error at 'super': Can't use 'super' in a class with no superclass.
      }
    }
    
    Base().foo();
    |};
  [%expect
    {| Evaluation error: can't reference "super" in a class with no superclass |}]

let%expect_test "no_superclass_method.lox" =
  test
    {|
    class Base {}

    class Derived < Base {
      foo() {
        super.doesNotExist(1); // expect runtime error: Undefined property 'doesNotExist'.
      }
    }
    
    Derived().foo();
    |};
  [%expect {| Evaluation error: undefined attribute doesNotExist |}]

let%expect_test "parenthesized.lox" =
  test
    {|
    class A {
      method() {}
    }
    
    class B < A {
      method() {
        // [line 8] Error at ')': Expect '.' after 'super'.
        (super).method();
      }
    }
    |};
  [%expect {| Syntax error at line 10, column 14 ()) (last token: () |}]

let%expect_test "reassign_superclass.lox" =
  test
    {|
    class Base {
      method() {
        print "Base.method()";
      }
    }
    
    class Derived < Base {
      method() {
        super.method();
      }
    }
    
    class OtherBase {
      method() {
        print "OtherBase.method()";
      }
    }
    
    var derived = Derived();
    derived.method(); // expect: Base.method()
    Base = OtherBase;
    derived.method(); // expect: Base.method()
    |};
  [%expect {|
    Base.method()
    Base.method() |}]

let%expect_test "super_at_top_level.lox" =
  test
    {|
    super.foo("bar"); // Error at 'super': Can't use 'super' outside of a class.
    super.foo; // Error at 'super': Can't use 'super' outside of a class.
    |};
  [%expect {| Evaluation error: can't reference "super" outside a class |}]

let%expect_test "super_in_closure_in_inherited_method.lox" =
  test
    {|
    class A {
      say() {
        print "A";
      }
    }
    
    class B < A {
      getClosure() {
        fun closure() {
          super.say();
        }
        return closure;
      }
    
      say() {
        print "B";
      }
    }
    
    class C < B {
      say() {
        print "C";
      }
    }
    
    C().getClosure()(); // expect: A
    |};
  [%expect {| A |}]

let%expect_test "super_in_inherited_method.lox" =
  test
    {|
    class A {
      say() {
        print "A";
      }
    }
    
    class B < A {
      test() {
        super.say();
      }
    
      say() {
        print "B";
      }
    }
    
    class C < B {
      say() {
        print "C";
      }
    }
    
    C().test(); // expect: A
    |};
  [%expect {| A |}]

let%expect_test "super_in_top_level_function.lox" =
  test
    {|
    super.bar(); // Error at 'super': Can't use 'super' outside of a class.
    fun foo() {
    }
    |};
  [%expect {| Evaluation error: can't reference "super" outside a class |}]

let%expect_test "super_without_dot.lox" =
  test
    {|
    class A {}

    class B < A {
      method() {
        // [line 6] Error at ';': Expect '.' after 'super'.
        super;
      }
    }
    |};
  [%expect {| Syntax error at line 8, column 13 (;) (last token: ;) |}]

let%expect_test "super_without_name.lox" =
  test
    {|
    class A {}

    class B < A {
      method() {
        super.; // Error at ';': Expect superclass method name.
      }
    }
    |};
  [%expect {| Syntax error at line 6, column 14 (;) (last token: ;) |}]

let%expect_test "this_in_superclass_method.lox" =
  test
    {|
    class Base {
      init(a) {
        this.a = a;
      }
    }
    
    class Derived < Base {
      init(a, b) {
        super.init(a);
        this.b = b;
      }
    }
    
    var derived = Derived("a", "b");
    print derived.a; // expect: a
    print derived.b; // expect: b
    |};
  [%expect {|
    a
    b |}]
