module Std = Typedvec
module AF = Typedvec.Ext.Affine
module S = Std.Size
module A = Std.Algebra
module M = A.Mat

let tests =
  [
    ( "Affine should have identity matrix after immediately making",
      `Quick,
      fun () ->
        let af = AF.make S.two in
        let m = A.Mat.identity S.three in
        let expected = A.Mat.to_list m in
        Alcotest.(check @@ list @@ list @@ float 0.0001) "to_list" expected (A.Mat.to_list (AF.to_mat af)) );
    ( "Affine can apply translation",
      `Quick,
      fun () ->
        let af = AF.make S.two in
        let v = A.Vec.make S.two 2.0 in
        let af = AF.translate af ~vec:v in
        let m = AF.to_mat af in
        let m_test = Alcotest.(option @@ float 0.0001) in
        Alcotest.(check m_test) "get" (Some 2.0) @@ M.get ~col:0 ~row:2 m;
        Alcotest.(check m_test) "get" (Some 2.0) @@ M.get ~col:1 ~row:2 m;
        Alcotest.(check m_test) "get" (Some 1.0) @@ M.get ~col:2 ~row:2 m );
    ( "Affine can apply multi translation",
      `Quick,
      fun () ->
        let af = AF.make S.two in
        let v = A.Vec.make S.two 2.0 in
        let af = AF.translate af ~vec:v |> AF.translate ~vec:v in
        let m = AF.to_mat af in
        let m_test = Alcotest.(option @@ float 0.0001) in
        Alcotest.(check m_test) "translate" (Some 4.0) @@ (M.get ~col:0 ~row:2 m [@eq Some 4.0]);
        Alcotest.(check m_test) "translate" (Some 4.0) @@ (M.get ~col:1 ~row:2 m [@eq Some 4.0]);
        Alcotest.(check m_test) "translate" (Some 1.0) @@ (M.get ~col:2 ~row:2 m [@eq Some 1.0]) );
    ( "Affine can apply scaling",
      `Quick,
      fun () ->
        let af = AF.make S.two in
        let v = A.Vec.make S.two 2.0 in
        let af = AF.scale af ~scale:v in
        let m = AF.to_mat af in
        let m_test = Alcotest.(option @@ float 0.0001) in
        Alcotest.(check m_test) "scale" (Some 2.0) @@ M.get ~col:0 ~row:0 m;
        Alcotest.(check m_test) "scale" (Some 2.0) @@ M.get ~col:1 ~row:1 m;
        Alcotest.(check m_test) "scale" (Some 1.0) @@ M.get ~col:2 ~row:2 m );
  ]
