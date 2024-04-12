open Lib

let%expect_test "assign_to_closure" =
  test
    {|
    var f = nil;
    var g = nil;
    
    {
      var local = "local";
      fun f_() {
        print local;
        local = "after f";
        print local;
      }
      f = f_;
    
      fun g_() {
        print local;
        local = "after g";
        print local;
      }
      g = g_;
    }
    
    f();
    // expect: local
    // expect: after f
    
    g();
    // expect: after f
    // expect: after g
    |};
  [%expect {|
    local
    after f
    after f
    after g |}]

let%expect_test "assign_to_shadowed_later" =
  test
    {|
    var a = "global";

    {
      fun assign() {
        a = "assigned";
      }
    
      var a = "inner";
      assign();
      print a; // expect: inner
    }
    
    print a; // expect: assigned
    |};
  [%expect {|
    inner
    assigned |}]

let%expect_test "close_over_function_parameter" =
  test
    {|
    var f = nil;

    fun foo(param) {
      fun f_() {
        print param;
      }
      f = f_;
    }
    foo("param");
    
    f(); // expect: param

    |};
  [%expect {| param |}]

let%expect_test "close_over_later_variable" =
  test
    {|
    // This is a regression test. There was a bug where if an upvalue for an
    // earlier local (here "a") was captured *after* a later one ("b"), then it
    // would crash because it walked to the end of the upvalue list (correct), but
    // then didn't handle not finding the variable.
    
    fun f() {
      var a = "a";
      var b = "b";
      fun g() {
        print b; // expect: b
        print a; // expect: a
      }
      g();
    }
    f();
    |};
  [%expect {|
    b
    a |}]

let%expect_test "close_over_method_parameter" =
  test
    {|
    var f = nil;

    class Foo {
      method(param) {
        fun f_() {
          print param;
        }
        f = f_;
      }
    }
    
    Foo().method("param");
    f(); // expect: param
    |};
  [%expect {| param |}]

let%expect_test "closed_closure_in_function" =
  test
    {|
    var f = nil;

    {
      var local = "local";
      fun f_() {
        print local;
      }
      f = f_;
    }
    
    f(); // expect: local

    |};
  [%expect {| local |}]

let%expect_test "nested_closure" =
  test
    {|
    var f = nil;

    fun f1() {
      var a = "a";
      fun f2() {
        var b = "b";
        fun f3() {
          var c = "c";
          fun f4() {
            print a;
            print b;
            print c;
          }
          f = f4;
        }
        f3();
      }
      f2();
    }
    f1();
    
    f();
    // expect: a
    // expect: b
    // expect: c
    |};
  [%expect {|
    a
    b
    c |}]

let%expect_test "open_closure_in_function" =
  test
    {|
    {
      var local = "local";
      fun f() {
        print local; // expect: local
      }
      f();
    }

    |};
  [%expect {| local |}]

let%expect_test "reference_closure_multiple_times" =
  test
    {|
    var f = nil;

    {
      var a = "a";
      fun f_() {
        print a;
        print a;
      }
      f = f_;
    }
    
    f();
    // expect: a
    // expect: a
    |};
  [%expect {|
    a
    a |}]

let%expect_test "reuse_closure_slot" =
  test
    {|
    {
      var f = nil;
    
      {
        var a = "a";
        fun f_() { print a; }
        f = f_;
      }
    
      {
        // Since a is out of scope, the local slot will be reused by b. Make sure
        // that f still closes over a.
        var b = "b";
        f(); // expect: a
      }
    }
    |};
  [%expect {| a |}]

let%expect_test "shadow_closure_with_local" =
  test
    {|
    {
      var foo = "closure";
      fun f() {
        {
          print foo; // expect: closure
          var foo = "shadow";
          print foo; // expect: shadow
        }
        print foo; // expect: closure
      }
      f();
    }
    |};
  [%expect {|
    closure
    shadow
    closure |}]

let%expect_test "unused_closure" =
  test
    {|
    // This is a regression test. There was a bug where the VM would try to close
    // an upvalue even if the upvalue was never created because the codepath for
    // the closure was not executed.
    
    {
      var a = "a";
      if (false) {
        fun foo() { a; }
      }
    }
    
    // If we get here, we didn't segfault when a went out of scope.
    print "ok"; // expect: ok

    |};
  [%expect {| ok |}]

let%expect_test "unused_later_closure" =
  test
    {|
    // This is a regression test. When closing upvalues for discarded locals, it
    // wouldn't make sure it discarded the upvalue for the correct stack slot.
    //
    // Here we create two locals that can be closed over, but only the first one
    // actually is. When "b" goes out of scope, we need to make sure we don't
    // prematurely close "a".
    var closure = nil;
    
    {
      var a = "a";
    
      {
        var b = "b";
        fun returnA() {
          return a;
        }
    
        closure = returnA;
    
        if (false) {
          fun returnB() {
            return b;
          }
        }
      }
    
      print closure(); // expect: a
    }
    |};
  [%expect {| a |}]
