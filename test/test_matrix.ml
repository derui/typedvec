module S = Typedvec.Size
module V = Typedvec.Vector
module M = Typedvec.Matrix

let%spec "Mat can make matrix with initialize" =
  let mat = M.make ~row:S.two ~col:S.two ~init:0.0 in
  (M.col_size mat |> S.to_int) [@eq 2];
  (M.row_size mat |> S.to_int) [@eq 2]

let%spec "Mat should raise Invalid_argument when given size less than 1" =
  (fun () -> M.make ~row:S.zero ~col:S.two ~init:0.0) [@raises Invalid_argument ("Matrix size must be greater than 1")]

let%spec "Mat can get row as vector" =
  let mat = M.make ~row:S.two ~col:S.two ~init:0.0 in
  let row = M.get_row ~row:0 mat in
  match row with
  | Some row -> V.to_list row [@eq [0.0;0.0]]
  | None -> failwith "Illegal result"

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
  let mat = M.init ~row:S.two ~col:S.three ~f:(fun r c -> (r,c)) in
  (M.get ~col:0 ~row:0 mat) [@eq Some (0,0)];
  (M.get ~col:1 ~row:0 mat) [@eq Some (0,1)];
  (M.get ~col:0 ~row:1 mat) [@eq Some (1,0)];
  (M.get ~col:1 ~row:1 mat) [@eq Some (1,1)];
  (M.get ~col:2 ~row:1 mat) [@eq Some (1,2)]
