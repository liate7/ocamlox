open Lib

let%expect_test "empty" =
  test
    {|
    {}

    if (true) {}
    if (false) {} else {}
    
    print "ok";
    |};
  [%expect {|ok|}]

let%expect_test "scope" =
  test
    {|
    var a = "outer";

    {
      var a = "inner";
      print a;
    }

    print a;
    |};
  [%expect {|
    inner
    outer
    |}]
