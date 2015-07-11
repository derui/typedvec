module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module V = A.Vec
module M = A.Mat

let%spec "Vec.cross should return cross product with 2 vectors" =
  let a = V.make S.three 0.0
  and b = V.make S.three 0.0 in
  List.iteri (fun ind v -> V.set a ind v) [1.0;2.0;0.0];
  List.iteri (fun ind v -> V.set b ind v) [0.0;1.0;-1.0];
  let c = V.cross ~left:a ~right:b in
  V.to_list c [@eq [-2.0;1.0;1.0]]

let%spec "Vec.add should return added v1 to v2" =
  let a = V.make S.three 2.0
  and b = V.make S.three 3.0 in
  let c = V.add a b in
  V.to_list c [@eq [5.0;5.0;5.0]]

let%spec "Vec.sub should return subtracted v2 from v1" =
  let a = V.init S.three (fun i -> (float_of_int i) *. 2.0)
  and b = V.init S.three (fun i -> (float_of_int i)  *. 3.0) in
  let c = V.sub a b in
  V.to_list c [@eq [0.0;-1.0;-2.0]]

let%spec "Vec.dot should return result of dot product" =
  let a = V.init S.three (fun i -> (float_of_int i) *. 2.0)
  and b = V.init S.three (fun i -> (float_of_int i)  *. 3.0) in
  V.dot a b [@eq 30.0]

let%spec "Vec.scaler should return result of multiply scalar" =
  let v = V.init S.three (fun i -> (float_of_int i) *. 1.0) in
  (V.scalar ~scale:2.0 ~v |> V.to_list) [@eq [0.0;2.0;4.0]]

(* For Matrix *)
let%spec "Mat.mul should return result of multply each matrixs" =
  let a = M.make ~col:S.two ~row:S.one ~init:0.0
  and b = M.make ~row:S.two ~col:S.one ~init:0.0 in
  M.set ~row:0 ~col:0 ~v:(-3.0) a;
  M.set ~row:0 ~col:1 ~v:4.0 a;
  M.set ~row:0 ~col:0 ~v:1.0 b;
  M.set ~row:1 ~col:0 ~v:2.0 b;
  let c = M.mul a b in
  M.get ~row:0 ~col:0 c [@eq Some (5.0)]

let%spec "Multiply identity with some matrix must equals original it" =
  let id = M.identity S.two in
  let mat = M.make ~row:S.two ~col:S.two ~init:10.0 in
  let mat = M.mul id mat in
  (M.row_of_mat ~row:0 mat) [@eq [10.0;10.0]];
  (M.row_of_mat ~row:1 mat) [@eq [10.0;10.0]];

  let mat = M.mul mat id in
  (M.row_of_mat ~row:0 mat) [@eq [10.0;10.0]];
  (M.row_of_mat ~row:1 mat) [@eq [10.0;10.0]]

let%spec "Mat.identity should return identity matrix with size" =
  let matrix = M.identity S.two in
  M.get ~row:0 ~col:0 matrix [@eq Some (1.0)];
  M.get ~row:0 ~col:1 matrix [@eq Some (0.0)];
  M.get ~row:1 ~col:0 matrix [@eq Some (0.0)];
  M.get ~row:1 ~col:1 matrix [@eq Some (1.0)]

let%spec "Mat.diagonal should return diagonal matrix with size" =
  let matrix = M.diagonal ~size:S.two ~comp:2.0 in
  M.get ~row:0 ~col:0 matrix [@eq Some (2.0)];
  M.get ~row:0 ~col:1 matrix [@eq Some (0.0)];
  M.get ~row:1 ~col:0 matrix [@eq Some (0.0)];
  M.get ~row:1 ~col:1 matrix [@eq Some (2.0)]

let%spec "Mat.scalar should return new matrix multiply scale" =
  let m = M.identity S.two in
  let m = M.scalar ~scale:3.0 ~m in
  M.get ~row:0 ~col:0 m [@eq Some (3.0)];
  M.get ~row:0 ~col:1 m [@eq Some (0.0)];
  M.get ~row:1 ~col:0 m [@eq Some (0.0)];
  M.get ~row:1 ~col:1 m [@eq Some (3.0)]

let%spec "Mat.add should return result to add two matrix" =
  let a = M.init ~row:S.two ~col:S.two ~f:(fun a b -> (succ a) * (succ b) |> float_of_int) in
  let b = M.init ~row:S.two ~col:S.two ~f:(fun a b -> succ a |> float_of_int) in
  let c = M.add a b in
  M.get ~row:0 ~col:0 c [@eq Some (2.0)];
  M.get ~row:0 ~col:1 c [@eq Some (3.0)];
  M.get ~row:1 ~col:0 c [@eq Some (4.0)];
  M.get ~row:1 ~col:1 c [@eq Some (6.0)]

let%spec "Mat.sub should return result to subtract two matrix" =
  let a = M.init ~row:S.two ~col:S.two ~f:(fun a b -> (succ a) * (succ b) |> float_of_int) in
  let b = M.make ~row:S.two ~col:S.two ~init:3.0 in
  let c = M.sub a b in
  M.get ~row:0 ~col:0 c [@eq Some (-2.0)];
  M.get ~row:0 ~col:1 c [@eq Some (-1.0)];
  M.get ~row:1 ~col:0 c [@eq Some (-1.0)];
  M.get ~row:1 ~col:1 c [@eq Some (1.0)]

let%spec "Mat.inverse should return invserse matrix if it have inverse matrix" =
  let a = M.make ~row:S.two ~col:S.two ~init:0.0 in
  M.set ~row:0 ~col:0 ~v:2.0 a;
  M.set ~row:0 ~col:1 ~v:5.0 a;
  M.set ~row:1 ~col:0 ~v:1.0 a;
  M.set ~row:1 ~col:1 ~v:3.0 a;
  let inv = M.inverse a in
  match inv with
  | None -> failwith ""
  | Some inv -> let inv = M.to_list inv in
                inv [@eq [[3.0;-5.0];
                      [-1.0;2.0]]
                ]

let%spec "Mat.det should return determinant of matrix" = 
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
  det [@eq Some (-7.0)]
