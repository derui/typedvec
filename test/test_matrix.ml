module Std = Typedvec
module S = Std.Size

module T = struct
  type num_type = float

  let compare = Stdlib.compare
end

module V = Std.Vec.Make (T)
module M = Std.Mat.Make (T)

let tests =
  [
    ( "Mat can make matrix with initialize",
      `Quick,
      fun () ->
        let mat = M.make ~row:S.two ~col:S.two ~init:0.0 in
        Alcotest.(check int) "cols" 2 (M.col_size mat |> S.to_int);
        Alcotest.(check int) "rows" 2 (M.row_size mat |> S.to_int) );
    ( "Mat should raise Invalid_argument when given size less than 1",
      `Quick,
      fun () ->
        Alcotest.check_raises "grater" (Invalid_argument "Matrix size must be greater than 1") (fun () ->
            M.make ~row:S.zero ~col:S.two ~init:0.0 |> ignore) );
    ( "Mat can access element at specified position",
      `Quick,
      fun () ->
        let mat = M.make ~row:S.two ~col:S.two ~init:1.0 in
        let v = M.get ~col:0 ~row:0 mat in
        Alcotest.(check @@ option @@ float 0.1) "access" (Some 1.0) v );
    ( "Mat can transpose some matrix",
      `Quick,
      fun () ->
        let mat = M.make ~row:S.two ~col:S.three ~init:0.0 in
        let mat = M.transpose mat in
        Alcotest.(check int) "rows" 3 (M.row_size mat |> S.to_int);
        Alcotest.(check int) "cols" 2 (M.col_size mat |> S.to_int) );
    ( "Matrix can make with function for index arguments",
      `Quick,
      fun () ->
        let module M = Std.Mat.Make (struct
          type num_type = int * int
        end) in
        let mat = M.init ~row:S.two ~col:S.three ~f:(fun r c -> (r, c)) in
        Alcotest.(check @@ option @@ pair int int) "0,0" (Some (0, 0)) (M.get ~col:0 ~row:0 mat);
        Alcotest.(check @@ option @@ pair int int) "1,0" (Some (0, 1)) (M.get ~col:1 ~row:0 mat);
        Alcotest.(check @@ option @@ pair int int) "0,1" (Some (1, 0)) (M.get ~col:0 ~row:1 mat);
        Alcotest.(check @@ option @@ pair int int) "1,1" (Some (1, 1)) (M.get ~col:1 ~row:1 mat);
        Alcotest.(check @@ option @@ pair int int) "2,1" (Some (1, 2)) (M.get ~col:2 ~row:1 mat) );
    ( "Matrix can set a value at specified position",
      `Quick,
      fun () ->
        let module M = Std.Mat.Make (struct
          type num_type = int * int
        end) in
        let mat = M.init ~row:S.two ~col:S.three ~f:(fun r c -> (r, c)) in
        M.set ~col:0 ~row:0 ~v:(100, 100) mat;
        Alcotest.(check @@ option @@ pair int int) "set" (Some (100, 100)) (M.get ~col:0 ~row:0 mat) );
    ( "Matrix should be apply function for each element of Matrix",
      `Quick,
      fun () ->
        let module M = Std.Mat.Make (struct
          type num_type = int
        end) in
        let mat = M.make ~row:S.two ~col:S.two ~init:1 in
        let r = M.map ~f:(fun row col _ -> (row * 10) + col) mat in
        Alcotest.(check @@ option int) "apply" (Some 0) (M.get ~row:0 ~col:0 r);
        Alcotest.(check @@ option int) "apply" (Some 1) (M.get ~row:0 ~col:1 r);
        Alcotest.(check @@ option int) "apply" (Some 10) (M.get ~row:1 ~col:0 r);
        Alcotest.(check @@ option int) "apply" (Some 11) (M.get ~row:1 ~col:1 r) );
    ( "Matrix can get row as list of the matrix",
      `Quick,
      fun () ->
        let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int ((10 * r) + c)) in

        Alcotest.(check @@ list @@ float 0.01) "row 1" [ 0.0; 1.0 ] (M.row_of_mat ~row:0 mat);
        Alcotest.(check @@ list @@ float 0.01) "row 2" [ 10.0; 11.0 ] (M.row_of_mat ~row:1 mat) );
    ( "Matrix can get column as list of the matrix",
      `Quick,
      fun () ->
        let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int ((10 * r) + c)) in

        Alcotest.(check @@ list @@ float 0.01) "col 1" [ 0.0; 10.0 ] (M.col_of_mat ~col:0 mat);
        Alcotest.(check @@ list @@ float 0.01) "col 2" [ 1.0; 11.0 ] (M.col_of_mat ~col:1 mat) );
    ( "Matrix can get submatrix from the matrix",
      `Quick,
      fun () ->
        let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int ((10 * r) + c)) in

        let () =
          match M.submatrix ~row:S.one ~col:S.one mat with
          | None      -> failwith "No any submatrix"
          | Some newm ->
              Alcotest.(check @@ option @@ float 0.1) "new mem" (Some 0.0) @@ M.get ~row:0 ~col:0 newm;
              Alcotest.(check int) "row size" 1 (M.row_size newm |> S.to_int);
              Alcotest.(check int) "col size" 1 (M.col_size newm |> S.to_int)
        in
        match M.submatrix ~start_row:1 ~start_col:1 ~row:S.one ~col:S.one mat with
        | None      -> failwith "No any submatrix"
        | Some newm ->
            Alcotest.(check @@ option @@ float 0.1) "sub matrix" (Some 11.0) @@ M.get ~row:0 ~col:0 newm;
            Alcotest.(check int) "col size" 1 (M.row_size newm |> S.to_int);
            Alcotest.(check int) "col size" 1 (M.col_size newm |> S.to_int) );
    ( "Matrix should return None if no have enough area to get submatrix",
      `Quick,
      fun () ->
        let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int ((10 * r) + c)) in

        Alcotest.(check @@ option @@ of_pp Fmt.nop) "invalid row" None @@ M.submatrix ~row:S.three ~col:S.one mat;
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "invalid columns" None @@ M.submatrix ~row:S.one ~col:S.three mat );
  ]
