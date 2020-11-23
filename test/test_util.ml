module T = Typedvec

let tests =
  [
    Alcotest.test_case "Util.range should be able to make integer list" `Quick (fun () ->
        let open T.Util in
        Alcotest.(check @@ list int) "empty" [] (range (-1));
        Alcotest.(check @@ list int) "empty" [] (range 0);
        Alcotest.(check @@ list int) "empty" [ 0 ] (range 1);
        Alcotest.(check @@ list int) "empty" [ 0; 1 ] (range 2));
  ]
