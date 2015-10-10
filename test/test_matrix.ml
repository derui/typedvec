module Std = Typedvec.Std
module S = Std.Size
module T = struct
  type num_type = float
  let compare = Pervasives.compare
end
module V = Std.Vec.Make(T)
module M = Std.Mat.Make(T)

let%spec "Mat can make matrix with initialize" =
  let mat = M.make ~row:S.two ~col:S.two ~init:0.0 in
  (M.col_size mat |> S.to_int) [@eq 2];
  (M.row_size mat |> S.to_int) [@eq 2]

let%spec "Mat should raise Invalid_argument when given size less than 1" =
  (fun () -> M.make ~row:S.zero ~col:S.two ~init:0.0) [@raises Invalid_argument ("Matrix size must be greater than 1")]

let%spec "Mat can access element at specified position" =
  let mat = M.make ~row:S.two ~col:S.two ~init:1.0 in
  let v = M.get ~col:0 ~row:0 mat in
  v [@eq Some 1.0]

let%spec "Mat can transpose some matrix" =
  let mat = M.make ~row:S.two ~col:S.three ~init:0.0 in
  let mat = M.transpose mat in
  (M.row_size mat |> S.to_int) [@eq 3];
  (M.col_size mat |> S.to_int) [@eq 2]

let%spec "Matrix can make with function for index arguments" =
  let module M = Std.Mat.Make(struct
    type num_type = int * int
  end) in
  let mat = M.init ~row:S.two ~col:S.three ~f:(fun r c -> (r,c)) in
  (M.get ~col:0 ~row:0 mat) [@eq Some (0,0)];
  (M.get ~col:1 ~row:0 mat) [@eq Some (0,1)];
  (M.get ~col:0 ~row:1 mat) [@eq Some (1,0)];
  (M.get ~col:1 ~row:1 mat) [@eq Some (1,1)];
  (M.get ~col:2 ~row:1 mat) [@eq Some (1,2)]

let%spec "Matrix can set a value at specified position" =
  let module M = Std.Mat.Make(struct
    type num_type = int * int
  end) in
  let mat = M.init ~row:S.two ~col:S.three ~f:(fun r c -> (r,c)) in
  M.set ~col:0 ~row:0 ~v:(100,100) mat;
  (M.get ~col:0 ~row:0 mat) [@eq Some (100,100)]

let%spec "Matrix should be apply function for each element of Matrix" =
  let module M = Std.Mat.Make(struct
    type num_type = int
  end) in
  let mat = M.make ~row:S.two ~col:S.two ~init:1 in
  let r = M.map ~f:(fun row col _ -> row * 10 + col) mat in
  (M.get ~row:0 ~col:0 r) [@eq Some (0)];
  (M.get ~row:0 ~col:1 r) [@eq Some (1)];
  (M.get ~row:1 ~col:0 r) [@eq Some (10)];
  (M.get ~row:1 ~col:1 r) [@eq Some (11)]

let%spec "Matrix can get row as list of the matrix" =
  let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int (10 * r + c)) in

  (M.row_of_mat ~row:0 mat) [@eq [0.0;1.0]];
  (M.row_of_mat ~row:1 mat) [@eq [10.0;11.0]]

let%spec "Matrix can get column as list of the matrix" =
  let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int (10 * r + c)) in

  (M.col_of_mat ~col:0 mat) [@eq [0.0;10.0]];
  (M.col_of_mat ~col:1 mat) [@eq [1.0;11.0]]

let%spec "Matrix can get submatrix from the matrix" =
  let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int (10 * r + c)) in

  match M.submatrix ~row:S.one ~col:S.one mat with
  | None -> failwith "No any submatrix"
  | Some(newm) -> begin
    (M.get ~row:0 ~col:0 newm) [@eq Some(0.0)];
    (M.row_size newm |> S.to_int) [@eq 1];
    (M.col_size newm |> S.to_int) [@eq 1]
  end;

  match M.submatrix ~start_row:1 ~start_col:1 ~row:S.one ~col:S.one mat with
  | None -> failwith "No any submatrix"
  | Some(newm) -> begin
    (M.get ~row:0 ~col:0 newm) [@eq Some(11.0)];
    (M.row_size newm |> S.to_int) [@eq 1];
    (M.col_size newm |> S.to_int) [@eq 1]
  end

let%spec "Matrix should return None if no have enough area to get submatrix" =
  let mat = M.init ~row:S.two ~col:S.two ~f:(fun r c -> float_of_int (10 * r + c)) in

  match M.submatrix ~row:S.three ~col:S.one mat with
  | Some(_) -> failwith "Illegal result"
  | None -> true [@eq true];

  match M.submatrix ~row:S.one ~col:S.three mat with
  | Some(_) -> failwith "Illegal result"
  | None -> true [@eq true]
