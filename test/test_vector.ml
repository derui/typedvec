module Std = Typedvec
module S = Std.Size

module V = Std.Vec.Make (struct
  type num_type = int

  let compare = Stdlib.compare
end)

let tests =
  [
    Alcotest.test_case "Make array from size" `Quick (fun () ->
        let v = V.make S.two 0 in
        Alcotest.(check int) "size" 2 (V.size v |> S.to_int);
        Alcotest.(check @@ list int) "vec" [ 0; 0 ] [ V.unsafe_get v 0; V.unsafe_get v 1 ]);
    Alcotest.test_case "Vec should raise Invalid_argument when make with given size less than 1" `Quick (fun () ->
        Alcotest.check_raises "invalid dimension" (Invalid_argument "Vector size must be greater than 1") (fun () ->
            V.make S.zero 0 |> ignore));
    Alcotest.test_case "Vec should raise Invalid_argument when init with given size less than 1" `Quick (fun () ->
        Alcotest.check_raises "invalid dimension" (Invalid_argument "Vector size must be greater than 1") (fun () ->
            V.init S.zero (fun _ -> 0) |> ignore));
    Alcotest.test_case "Vec can manipulate each element" `Quick (fun () ->
        let v = V.make S.two 0 in
        V.unsafe_set v 0 100;
        Alcotest.(check int) "element" 100 @@ V.unsafe_get v 0);
    Alcotest.test_case "Vec can manipulate each element with safe function" `Quick (fun () ->
        let v = V.make S.two 0 in
        V.set v ~index:0 ~v:100;
        Alcotest.(check @@ option int) "safe get 1" (Some 100) @@ V.get v ~index:0;
        V.set v ~index:(-1) ~v:10;
        Alcotest.(check @@ option int) "safe get -1" None @@ V.get v ~index:(-1);
        V.set v ~index:2 ~v:20;
        Alcotest.(check @@ option int) "safe get 2" None @@ V.get v ~index:2);
    Alcotest.test_case "Vec can convert to list" `Quick (fun () ->
        let v = V.init S.two (( * ) 2) in
        let ary = V.to_list v in
        Alcotest.(check @@ list int) "initialize" [ 0; 2 ] ary);
    Alcotest.test_case "Vec can be mapping with some function" `Quick (fun () ->
        let v = V.init S.two (fun i -> succ i) in
        Alcotest.(check @@ list int) "map" [ 2; 3 ] (V.map v ~f:(fun e -> succ e) |> V.to_list);
        Alcotest.(check @@ list @@ float 0.001) "map" [ 1.0; 2.0 ] (V.map v ~f:float_of_int |> V.to_list);
        Alcotest.(check @@ list int) "map" [ 0; 1 ] (V.mapi v ~f:(fun i _ -> i) |> V.to_list));
    Alcotest.test_case "Vec can iterate with some function" `Quick (fun () ->
        let v = V.init S.two (fun i -> succ i) in
        let r = ref 0 in
        V.iter v ~f:(fun e -> r := !r + e);
        Alcotest.(check int) "iterate" 3 !r;
        r := 0;
        V.iteri v ~f:(fun i _ -> r := !r + pred i);
        Alcotest.(check int) "iterate" (-1) !r);
    Alcotest.test_case "Vec can fold left to any vector" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) in
        Alcotest.(check int) "fold_left" 6 (V.fold_left ~f:(fun s e -> s + e) ~init:0 v);
        Alcotest.(check @@ list int) "fold_left" [ 3; 2; 1 ] (V.fold_left ~f:(fun s e -> e :: s) ~init:[] v));
    Alcotest.test_case "Vec can fold right to any vector" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) in
        Alcotest.(check int) "fold_right" 6 (V.fold_right ~f:(fun s e -> s + e) ~init:0 v);
        Alcotest.(check @@ list int) "fold_right" [ 1; 2; 3 ] (V.fold_right ~f:(fun e s -> e :: s) ~init:[] v));
    Alcotest.test_case "Vec can predicate for all element" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) in
        Alcotest.(check bool) "for_all" true @@ V.for_all ~f:(fun e -> e > 0) v;
        Alcotest.(check bool) "for_all" false @@ V.for_all ~f:(fun e -> e > 1) v;
        let v = V.make S.one 0 in
        Alcotest.(check bool) "for_all" false (V.for_all ~f:(fun e -> e > 0) v));
    Alcotest.test_case "Vec can detect to exist element that is predicated or not" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) in
        Alcotest.(check bool) "exists" true @@ V.exists ~f:(fun e -> e > 0) v;
        Alcotest.(check bool) "exists" true @@ V.exists ~f:(fun e -> e = 3) v;
        Alcotest.(check bool) "exists" false @@ V.exists ~f:(fun e -> e < 1) v;
        let v = V.make S.one 0 in
        Alcotest.(check bool) "exists" true @@ V.exists ~f:(fun e -> e = 0) v);
    Alcotest.test_case "Vec can be contained member in the vector" `Quick (fun () ->
        let v = V.init S.four (fun i -> succ i) in
        Alcotest.(check bool) "member" true @@ V.mem ~member:1 v;
        Alcotest.(check bool) "member" false @@ V.mem ~member:0 v;
        let module V = Std.Vec.Make (struct
          type num_type = int list

          let compare = Stdlib.compare
        end) in
        let v = V.init S.four (fun i -> [ i ]) in
        Alcotest.(check bool) "member" false @@ V.memq ~member:[ 1 ] v);
    Alcotest.test_case "Vec can do map iteration with 2 vectors" `Quick (fun () ->
        let v1 = V.init S.three (fun i -> succ i) and v2 = V.init S.three (fun i -> pred i) in
        let mapped = V.map2 ~f:(fun a b -> a * b) ~v1 ~v2 in
        Alcotest.(check @@ list int) "to_list" [ -1; 0; 3 ] (V.to_list mapped));
    Alcotest.test_case "Vec can do iteration with 2 vectors" `Quick (fun () ->
        let v1 = V.init S.three (fun i -> succ i) and v2 = V.init S.three (fun i -> i + 2) in
        let r = ref 0 in
        V.iter2 ~f:(fun a b -> r := !r + (a * b)) ~v1 ~v2;
        Alcotest.(check int) "iter2" 20 !r);
    Alcotest.test_case "Vec can zip 2 vectors to one vector" `Quick (fun () ->
        let v1 = V.init S.three (fun i -> succ i) and v2 = V.init S.three (fun i -> i + 2) in
        let zipped = V.zip ~v1 ~v2 in
        Alcotest.(check @@ list @@ pair int int) "zip" [ (1, 2); (2, 3); (3, 4) ] (V.to_list zipped));
    Alcotest.test_case "Vec can copy to new allocated vector" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) in
        Alcotest.(check @@ list int) "copy" [ 1; 2; 3 ] (V.copy v |> V.to_list));
    Alcotest.test_case "Vec can copy to already allocated vector" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) and v2 = V.make S.three (-1) in
        V.copy ~y:v2 v |> ignore;
        Alcotest.(check @@ list int) "copy" [ 1; 2; 3 ] @@ V.to_list v2);
    Alcotest.test_case "Vec can detect equality vectors" `Quick (fun () ->
        let v = V.init S.three (fun i -> succ i) and v2 = V.init S.three (fun i -> succ i) and v3 = V.make S.three 1 in

        Alcotest.(check bool) "equal" true @@ V.equals v v2;
        Alcotest.(check bool) "equal" true @@ V.equals v2 v;
        Alcotest.(check bool) "equal" false @@ V.equals v v3;
        Alcotest.(check bool) "equal" false @@ V.equals v2 v3);
  ]
