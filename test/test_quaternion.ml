module Std = Typedvec
module S = Std.Size
module V = Std.Algebra.Vec
module Q = Std.Ext.Qua

let tests =
  [
    Alcotest.test_case "Quaternion should make a quaternion having specified angle and axis" `Quick (fun () ->
        let sub n v v' =
          let v = V.unsafe_get v n and v' = V.unsafe_get v' n in
          abs_float (v -. v') < 0.00001
        in
        let axis = V.make S.three 0.0 in
        V.set ~index:0 ~v:1.0 axis;
        V.set ~index:1 ~v:2.0 axis;
        V.set ~index:2 ~v:3.0 axis;
        let v = Q.make ~angle:10.0 ~axis () in
        Alcotest.(check bool) "angle" true (abs_float (10.0 -. Q.angle v) < 0.00001);
        let axis = V.normalize axis in
        let axis' = Q.axis v in
        Alcotest.(check bool) "angle" true @@ sub 0 axis' axis;
        Alcotest.(check bool) "angle" true @@ sub 1 axis' axis;
        Alcotest.(check bool) "angle" true @@ sub 2 axis' axis);
    Alcotest.test_case "Quaternion can return identity" `Quick (fun () ->
        let v = Q.identity in
        Alcotest.(check @@ list @@ float 0.01) "axis" [ 0.0; 0.0; 0.0 ] (Q.axis v |> V.to_list);
        Alcotest.(check @@ float 0.01) "angle" 0.0 @@ Q.angle v);
  ]
