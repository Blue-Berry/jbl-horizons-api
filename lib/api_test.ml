open! Core

let%expect_test "parse metadata" =
  let result = Api.SBQuery.(to_query_string [ EQ (A, 0.0) ]) in
  print_s [%sexp (result : string)];
  [%expect {| NAME%20=%20stuff%3B |}]
;;
