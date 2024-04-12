open Lib

let%expect_test "394.lox" =
  test
    {|
    {
      class A {}
      class B < A {}
      print B; // expect: B
    }
    |};
  [%expect {| #<class B> |}]

let%expect_test "40.lox" =
  test
    {|
    fun caller(g) {
      g();
      // g should be a function, not nil.
      print g == nil; // expect: false
    }
    
    fun callCaller() {
      var capturedVar = "before";
      var a = "a";
    
      fun f() {
        // Commenting the next line out prevents the bug!
        capturedVar = "after";
    
        // Returning anything also fixes it, even nil:
        //return nil;
      }
    
      caller(f);
    }
    
    callCaller();
   |};
  [%expect {| false |}]
