module T = Typedvec

let tests =
  [
    Alcotest.test_case "Size to int" `Quick (fun () ->
        let open T.Size in
        Alcotest.(check int) "size" 1 (to_int one);
        Alcotest.(check int) "size" 2 (to_int two);
        Alcotest.(check int) "size" 3 (to_int three);
        Alcotest.(check int) "size" 4 (to_int four);
        Alcotest.(check int) "size" 5 (to_int five);
        Alcotest.(check int) "size" 6 (to_int six);
        Alcotest.(check int) "size" 7 (to_int seven);
        Alcotest.(check int) "size" 8 (to_int eight);
        Alcotest.(check int) "size" 9 (to_int nine);
        Alcotest.(check int) "size" 10 (to_int ten));
    Alcotest.test_case "Size can check zero or not" `Quick (fun () ->
        let open T.Size in
        Alcotest.(check bool) "zero equals to zero" true @@ iszero zero;
        Alcotest.(check bool) "two not equals to zero" false @@ iszero two);
    Alcotest.test_case "Size can convert from int to type of Size" `Quick (fun () ->
        let open T.Size in
        let module M = (val of_int 10) in
        Alcotest.(check int) "to_int" 10 @@ M.to_int;
        Alcotest.(check bool) "to_int" true @@ (M.to_int = to_int ten));
  ]
