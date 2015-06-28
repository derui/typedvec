module Std = Typedvec.Std
module S = Std.Size
module T = struct
  type num_type = float
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

