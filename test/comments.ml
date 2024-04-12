open Lib

let%expect_test "line_at_eof.lox" =
  test {|
    print "ok"; // expect: ok
    // comment|};
  [%expect {| ok |}]

let%expect_test "only_line_comment.lox" =
  test {|
    // comment
    |};
  [%expect {||}]

let%expect_test "only_line_comment_and_line.lox" =
  test {|
    // comment

    |};
  [%expect {||}]

let%expect_test "unicode.lox" =
  test
    {|
    // Unicode characters are allowed in comments.
    //
    // Latin 1 Supplement: £§¶ÜÞ
    // Latin Extended-A: ĐĦŋœ
    // Latin Extended-B: ƂƢƩǁ
    // Other stuff: ឃᢆ᯽₪ℜ↩⊗┺░
    // Emoji: ☃☺♣
    
    print "ok"; // expect: ok
    |};
  [%expect {| ok |}]
