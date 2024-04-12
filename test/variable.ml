open Lib

let%expect_test "collide_with_parameter.lox" =
  test
    {|
    fun foo(a) {
      var a = "shadowing"; 
      print a;
    }
    foo("nope");
    |};
  [%expect {| shadowing |}]

let%expect_test "duplicate_local.lox" =
  test
    {|
    {
      var a = "value";
      var a = "other";
      print a;
    }
    |};
  [%expect {| other |}]

let%expect_test "duplicate_parameter.lox" =
  test
    {|
    fun foo(arg,
            arg) { // Error at 'arg': Already a variable with this name in this scope.
      "body";
    }
    |};
  [%expect
    {| Evaluation error: can't write a function with duplicate parameters (parameters: arg, arg) |}]

let%expect_test "early_bound.lox" =
  test
    {|
    var a = "outer";
    {
      fun foo() {
        print a;
      }
    
      foo(); // expect: outer
      var a = "inner";
      foo(); // expect: outer
    }
    |};
  [%expect {|
    outer
    outer |}]

let%expect_test "in_middle_of_block.lox" =
  test
    {|
    {
      var a = "a";
      print a; // expect: a
      var b = a + " b";
      print b; // expect: a b
      var c = a + " c";
      print c; // expect: a c
      var d = b + " d";
      print d; // expect: a b d
    }
    |};
  [%expect {|
    a
    a b
    a c
    a b d |}]

let%expect_test "in_nested_block.lox" =
  test
    {|
    {
      var a = "outer";
      {
        print a; // expect: outer
      }
    }
    |};
  [%expect {| outer |}]

let%expect_test "local_from_method.lox" =
  test
    {|
    var foo = "variable";

    class Foo {
      method() {
        print foo;
      }
    }
    
    Foo().method(); // expect: variable
    |};
  [%expect {| variable |}]

let%expect_test "redeclare_global.lox" =
  test {|
    var a = "1";
    var a = nil;
    print a; // expect: nil
    |};
  [%expect {| nil |}]

let%expect_test "redefine_global.lox" =
  test {|
    var a = "1";
    var a = "2";
    print a; // expect: 2
    |};
  [%expect {| 2 |}]

let%expect_test "scope_reuse_in_different_blocks.lox" =
  test
    {|
    {
      var a = "first";
      print a; // expect: first
    }
    
    {
      var a = "second";
      print a; // expect: second
    }
    |};
  [%expect {|
    first
    second |}]

let%expect_test "shadow_and_local.lox" =
  test
    {|
    {
      var a = "outer";
      {
        print a; // expect: outer
        var a = "inner";
        print a; // expect: inner
      }
    }
    |};
  [%expect {|
    outer
    inner |}]

let%expect_test "shadow_global.lox" =
  test
    {|
    var a = "global";
    {
      var a = "shadow";
      print a; // expect: shadow
    }
    print a; // expect: global
    |};
  [%expect {|
    shadow
    global |}]

let%expect_test "shadow_local.lox" =
  test
    {|
    {
      var a = "local";
      {
        var a = "shadow";
        print a; // expect: shadow
      }
      print a; // expect: local
    }
    |};
  [%expect {|
    shadow
    local |}]

let%expect_test "undefined_global.lox" =
  test
    {|
    print notDefined;  // expect runtime error: Undefined variable 'notDefined'.
    |};
  [%expect
    {| Evaluation error: tried to reference unbound variable notDefined |}]

let%expect_test "undefined_local.lox" =
  test
    {|
    {
      print notDefined;  // expect runtime error: Undefined variable 'notDefined'.
    }
    |};
  [%expect
    {| Evaluation error: tried to reference unbound variable notDefined |}]

let%expect_test "unreached_undefined.lox" =
  test
    {|
    if (false) {
      print notDefined;
    }
    
    print "ok"; // expect: ok
    |};
  [%expect {| ok |}]

let%expect_test "use_false_as_var.lox" =
  test
    {|
    // [line 2] Error at 'false': Expect variable name.
    var false = "value";
    |};
  [%expect {| Syntax error at line 4, column 8 (false) (last token: false) |}]

let%expect_test "use_global_in_initializer.lox" =
  test
    {|
    var a = "value";
    var a = a;
    print a; // expect: value
    |};
  [%expect {| value |}]

let%expect_test "use_local_in_initializer.lox" =
  test
    {|
    var a = "outer";
    {
      var a = a; // Error at 'a': Can't read local variable in its own initializer.
    }
    |};
  [%expect {| Evaluation error: can't reference "a" while shadowing it. |}]

let%expect_test "use_nil_as_var.lox" =
  test
    {|
    // [line 2] Error at 'nil': Expect variable name.
    var nil = "value";
    |};
  [%expect {| Syntax error at line 4, column 8 (nil) (last token: nil) |}]

let%expect_test "use_this_as_var.lox" =
  test
    {|
    // [line 2] Error at 'this': Expect variable name.
    var this = "value";
    |};
  [%expect {| Syntax error at line 4, column 8 (this) (last token: this) |}]
