module Std = Typedvec
module S = Std.Size
module V = Std.Algebra.Vec

let tests =
  [
    ( "Make vector with extension",
      `Quick,
      fun () ->
        let v = [%vec [ 1.0; 2.0 ]] in
        Alcotest.(check int) "size" 2 (V.size v |> S.to_int);
        Alcotest.(check @@ list @@ float 0.01) "to int" [ 1.0; 2.0 ] @@ V.to_list v );
    ( "Made vector with extension can compare via self make vector",
      `Quick,
      fun () ->
        let v = [%vec [ 1.0; 2.0 ]] in
        let v2 = V.init S.two (fun i -> succ i |> float_of_int) in
        Alcotest.(check bool) "equals" true @@ V.equals v v2 );
    ( "Vector made extension can place in expression",
      `Quick,
      fun () ->
        let v2 = V.init S.two (fun i -> succ i |> float_of_int) in
        Alcotest.(check bool) "not equal" true @@ V.equals [%vec [ 1.0; 2.0 ]] v2 );
  ]
