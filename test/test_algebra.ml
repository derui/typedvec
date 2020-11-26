module A = Typedvec.Algebra
module S = Typedvec.Size
module V = A.Vec
module M = A.Mat

let vec_test = Alcotest.(list @@ float 0.00001)

let mat_elm_test = Alcotest.(option @@ float 0.00001)

let tests =
  [
    ( "Vec.cross should return cross product with 2 vectors",
      `Quick,
      fun () ->
        let a = V.make S.three 0.0 and b = V.make S.three 0.0 in
        List.iteri (fun ind v -> V.set a ~index:ind ~v) [ 1.0; 2.0; 0.0 ];
        List.iteri (fun ind v -> V.set b ~index:ind ~v) [ 0.0; 1.0; -1.0 ];
        let c = V.cross ~left:a ~right:b in
        Alcotest.(check @@ list @@ float 0.1) "to_list" [ -2.0; 1.0; 1.0 ] @@ V.to_list c );
    ( "Vec.add should return added v1 to v2",
      `Quick,
      fun () ->
        let a = V.make S.three 2.0 and b = V.make S.three 3.0 in
        let c = V.add a b in

        Alcotest.(check @@ list @@ float 0.1) "added" [ 5.0; 5.0; 5.0 ] @@ V.to_list c );
    ( "Vec.zero should return zero vector",
      `Quick,
      fun () ->
        let a = V.zero S.three in
        Alcotest.(check vec_test) "zero" [ 0.0; 0.0; 0.0 ] @@ V.to_list a );
    ( "Vec.sub should return subtracted v2 from v1",
      `Quick,
      fun () ->
        let a = V.init S.three (fun i -> float_of_int i *. 2.0)
        and b = V.init S.three (fun i -> float_of_int i *. 3.0) in
        let c = V.sub a b in
        Alcotest.(check vec_test) "subtract" [ 0.0; -1.0; -2.0 ] @@ V.to_list c );
    ( "Vec.dot should return result of dot product",
      `Quick,
      fun () ->
        let a = V.init S.three (fun i -> float_of_int i *. 2.0)
        and b = V.init S.three (fun i -> float_of_int i *. 3.0) in
        Alcotest.(check @@ float 0.0001) "dot" 30.0 @@ V.dot a b );
    ( "Vec.scaler should return result of multiply scalar",
      `Quick,
      fun () ->
        let v = V.init S.three (fun i -> float_of_int i *. 1.0) in
        Alcotest.(check vec_test) "scalar" [ 0.0; 2.0; 4.0 ] (V.scalar ~scale:2.0 v |> V.to_list) );
    ( "Vec.norm should return norm of the vector",
      `Quick,
      fun () ->
        let v = V.init S.three (fun i -> float_of_int i +. 1.0) in
        Alcotest.(check @@ float 0.00001) "norm sqrt" (sqrt 14.0) (V.norm_sqrt v);
        Alcotest.(check @@ float 0.00001) "norm" 14.0 (V.norm v) );
    ( "Vec.normalize should return normalized vector",
      `Quick,
      fun () ->
        let v = V.init S.two (fun _ -> 1.0) in
        let l = 1.0 /. sqrt 2.0 in
        Alcotest.(check vec_test) "normalize" [ l; l ] (V.normalize v |> V.to_list) );
    (* For Matrix *)
    ( "Mat.mul should return result of multply each matrixs",
      `Quick,
      fun () ->
        let a = M.make ~col:S.two ~row:S.one ~init:0.0 and b = M.make ~row:S.two ~col:S.one ~init:0.0 in
        M.set ~row:0 ~col:0 ~v:(-3.0) a;
        M.set ~row:0 ~col:1 ~v:4.0 a;
        M.set ~row:0 ~col:0 ~v:1.0 b;
        M.set ~row:1 ~col:0 ~v:2.0 b;
        let c = M.mul a b in
        Alcotest.(check @@ option @@ float 0.001) "multiply" (Some 5.0) @@ M.get ~row:0 ~col:0 c );
    ( "Multiply identity with some matrix must equals original it",
      `Quick,
      fun () ->
        let id = M.identity S.two in
        let mat = M.make ~row:S.two ~col:S.two ~init:10.0 in
        let mat = M.mul id mat in
        Alcotest.(check vec_test) "row" [ 10.0; 10.0 ] (M.row_of_mat ~row:0 mat);
        Alcotest.(check vec_test) "col" [ 10.0; 10.0 ] (M.row_of_mat ~row:1 mat);
        let mat = M.mul mat id in
        Alcotest.(check vec_test) "row" [ 10.0; 10.0 ] (M.row_of_mat ~row:0 mat);
        Alcotest.(check vec_test) "col" [ 10.0; 10.0 ] (M.row_of_mat ~row:1 mat) );
    ( "Mat.identity should return identity matrix with size",
      `Quick,
      fun () ->
        let matrix = M.identity S.two in
        Alcotest.(check mat_elm_test) "(0,0)" (Some 1.0) @@ M.get ~row:0 ~col:0 matrix;
        Alcotest.(check mat_elm_test) "(0,1)" (Some 0.0) @@ M.get ~row:0 ~col:1 matrix;
        Alcotest.(check mat_elm_test) "(1,0)" (Some 0.0) @@ M.get ~row:1 ~col:0 matrix;
        Alcotest.(check mat_elm_test) "(1,1)" (Some 1.0) @@ M.get ~row:1 ~col:1 matrix );
    ( "Mat.diagonal should return diagonal matrix with size",
      `Quick,
      fun () ->
        let matrix = M.diagonal ~comp:2.0 S.two in
        Alcotest.(check mat_elm_test) "(0,0)" (Some 2.0) @@ M.get ~row:0 ~col:0 matrix;
        Alcotest.(check mat_elm_test) "(0,1)" (Some 0.0) @@ M.get ~row:0 ~col:1 matrix;
        Alcotest.(check mat_elm_test) "(1,0)" (Some 0.0) @@ M.get ~row:1 ~col:0 matrix;
        Alcotest.(check mat_elm_test) "(1,1)" (Some 2.0) @@ M.get ~row:1 ~col:1 matrix );
    ( "Mat.scalar should return new matrix multiply scale",
      `Quick,
      fun () ->
        let m = M.identity S.two in
        let m = M.scalar ~scale:3.0 m in
        Alcotest.(check mat_elm_test) "(0,0)" (Some 3.0) @@ M.get ~row:0 ~col:0 m;
        Alcotest.(check mat_elm_test) "(0,1)" (Some 0.0) @@ M.get ~row:0 ~col:1 m;
        Alcotest.(check mat_elm_test) "(1,0)" (Some 0.0) @@ M.get ~row:1 ~col:0 m;
        Alcotest.(check mat_elm_test) "(1,1)" (Some 3.0) @@ M.get ~row:1 ~col:1 m );
    ( "Mat.add should return result to add two matrix",
      `Quick,
      fun () ->
        let a = M.init ~row:S.two ~col:S.two ~f:(fun a b -> succ a * succ b |> float_of_int) in
        let b = M.init ~row:S.two ~col:S.two ~f:(fun a _ -> succ a |> float_of_int) in
        let c = M.add a b in
        Alcotest.(check mat_elm_test) "(0,0)" (Some 2.0) @@ M.get ~row:0 ~col:0 c;
        Alcotest.(check mat_elm_test) "(0,1)" (Some 3.0) @@ M.get ~row:0 ~col:1 c;
        Alcotest.(check mat_elm_test) "(1,0)" (Some 4.0) @@ M.get ~row:1 ~col:0 c;
        Alcotest.(check mat_elm_test) "(1,1)" (Some 6.0) @@ M.get ~row:1 ~col:1 c );
    ( "Mat.sub should return result to subtract two matrix",
      `Quick,
      fun () ->
        let a = M.init ~row:S.two ~col:S.two ~f:(fun a b -> succ a * succ b |> float_of_int) in
        let b = M.make ~row:S.two ~col:S.two ~init:3.0 in
        let c = M.sub a b in
        Alcotest.(check mat_elm_test) "(0,0)" (Some (-2.0)) @@ M.get ~row:0 ~col:0 c;
        Alcotest.(check mat_elm_test) "(0,1)" (Some (-1.0)) @@ M.get ~row:0 ~col:1 c;
        Alcotest.(check mat_elm_test) "(1,0)" (Some (-1.0)) @@ M.get ~row:1 ~col:0 c;
        Alcotest.(check mat_elm_test) "(1,1)" (Some 1.0) @@ M.get ~row:1 ~col:1 c );
    ( "Mat.inverse should return invserse matrix if it have inverse matrix",
      `Quick,
      fun () ->
        let a = M.make ~row:S.two ~col:S.two ~init:0.0 in
        M.set ~row:0 ~col:0 ~v:2.0 a;
        M.set ~row:0 ~col:1 ~v:5.0 a;
        M.set ~row:1 ~col:0 ~v:1.0 a;
        M.set ~row:1 ~col:1 ~v:3.0 a;
        let inv = M.inverse a in
        match inv with
        | None     -> failwith ""
        | Some inv ->
            let inv = M.to_list inv in
            Alcotest.(check @@ list @@ list @@ float 0.00001) "invert" [ [ 3.0; -5.0 ]; [ -1.0; 2.0 ] ] inv );
    ( "Mat.det should return determinant of matrix",
      `Quick,
      fun () ->
        let a = M.make ~row:S.three ~col:S.three ~init:0.0 in
        M.set ~row:0 ~col:0 ~v:3.0 a;
        M.set ~row:0 ~col:1 ~v:4.0 a;
        M.set ~row:0 ~col:2 ~v:(-1.0) a;
        M.set ~row:1 ~col:0 ~v:2.0 a;
        M.set ~row:1 ~col:1 ~v:5.0 a;
        M.set ~row:1 ~col:2 ~v:(-2.0) a;
        M.set ~row:2 ~col:0 ~v:1.0 a;
        M.set ~row:2 ~col:1 ~v:6.0 a;
        M.set ~row:2 ~col:2 ~v:(-4.0) a;
        let det = M.det a in
        Alcotest.(check mat_elm_test) "determinant" (Some (-7.0)) det );
    ( "mul_v2m should return multiply matrix with vector",
      `Quick,
      fun () ->
        let m = M.make ~row:S.two ~col:S.two ~init:1.0 in
        let v = V.make S.two 2.0 in
        let v' = A.mul_v2m v m in
        Alcotest.(check vec_test) "to_vec" [ 4.0; 4.0 ] (V.to_list v') );
    ( "mul_m2v should return multiply vector with matrix",
      `Quick,
      fun () ->
        let m = M.init ~row:S.two ~col:S.two ~f:(fun r _ -> float_of_int r +. 1.0) in
        let v = V.make S.two 2.0 in
        let v' = A.mul_m2v m v in
        Alcotest.(check vec_test) "to_vec" [ 4.0; 8.0 ] (V.to_list v') );
    ( "jacobi should get the values of unknown quantity",
      `Quick,
      fun () ->
        let m = M.make ~row:S.two ~col:S.two ~init:0.0 in
        M.set m ~row:0 ~col:0 ~v:2.0;
        M.set m ~row:0 ~col:1 ~v:1.0;
        M.set m ~row:1 ~col:0 ~v:1.0;
        M.set m ~row:1 ~col:1 ~v:5.0;
        let v = V.make S.two 0.0 in
        V.set v ~index:0 ~v:4.0;
        V.set v ~index:1 ~v:11.0;
        let ret = A.jacobi ~coefficient:m ~const:v () in
        let ret = V.to_list ret in
        let r = List.nth ret 1 in
        Alcotest.(check bool) "jacobi" true (r >= 1.99999 && r <= 2.0) );
  ]
