open Core
module Version = Semantic_version.Make ()

let%expect_test "Successfully parse well-formed semantic versions" =
  let test s = Version.of_string s |> (ignore : Version.t -> unit) in
  test "0.0.0";
  test "0.1.0";
  test "0.4.2-alpha+201410070245";
  test "0.6.0-dev+32-xy01abcde3";
  test "0.1.0+build-metadata-is.1ess-r1str1ct1v3.than-pre-release.01";
  [%expect {| |}]
;;

let%expect_test "Fail to parse badly-formed semantic versions" =
  let test s =
    match Version.of_string s with
    | version ->
      raise_s [%message "Successfully parsed badly-formed version" (version : Version.t)]
    | exception _ -> ()
  in
  test "";
  test "abc";
  test "0.";
  test "0.1.0.";
  test "v0.1.0";
  test "1";
  test "1.0";
  test "0.1.0-pre-release-is.m0re-r1str1ct1v3.than-build-metadata.01";
  [%expect {| |}]
;;

let%expect_test "Test precedence" =
  [ "1.0.0-beta"
  ; "1.11.0"
  ; "1.0.0-rc.1"
  ; "1.0.0"
  ; "1.0.0-beta.11"
  ; "1.0.0-alpha.1"
  ; "1.0.11"
  ; "1.0.2"
  ; "11.0.0"
  ; "1.0.0-beta.2"
  ; "1.0.0-alpha.beta"
  ; "1.2.0"
  ; "1.0.0-alpha"
  ; "0.0.11"
  ; "2.0.0"
  ; "0.0.2"
  ]
  |> List.map ~f:Version.of_string
  |> List.sort ~compare:Version.compare
  |> List.iter ~f:(printf !"%{Version}\n");
  [%expect
    {|
    0.0.2
    0.0.11
    1.0.0-alpha
    1.0.0-alpha.1
    1.0.0-alpha.beta
    1.0.0-beta
    1.0.0-beta.2
    1.0.0-beta.11
    1.0.0-rc.1
    1.0.0
    1.0.2
    1.0.11
    1.2.0
    1.11.0
    2.0.0
    11.0.0
    |}]
;;

let%expect_test "Build metadata is ignored when comparing versions" =
  let test v1 v2 = assert (Version.equal (Version.of_string v1) (Version.of_string v2)) in
  test "0.1.0-alpha+foo" "0.1.0-alpha+bar";
  test "0.1.0" "0.1.0+foo";
  [%expect {| |}]
;;

let%expect_test "Next versions" =
  let version = Version.of_string "0.1.0-alpha" in
  print_s [%sexp (Version.next_patch_release version : Version.t)];
  [%expect {| 0.1.1 |}];
  print_s [%sexp (Version.next_minor_release version : Version.t)];
  [%expect {| 0.2.0 |}];
  print_s [%sexp (Version.next_major_release version : Version.t)];
  [%expect {| 1.0.0 |}]
;;
