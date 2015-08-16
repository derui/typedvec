module Vec = Algebra_vec
module Mat = Algebra_mat

module S = Size
type num_type' = float
type num_type = num_type'

type (+'row, +'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t
type 'a s = 'a Size.t

let list_to_vec l s =
  let vec = Vec.make s 0.0 in
  List.iteri (fun idx v -> Vec.set vec idx v) l;
  vec

let mul_v2m v m =
  let size = Mat.col_size m in
  let row_size = Mat.row_size m in
  let v' = Vec.make size 0.0 in
  let col_range = Mat.col_size m |> Size.to_int |> Util.range in
  List.iter (fun col ->
    let cols = list_to_vec (Mat.col_of_mat ~col m) row_size in
    Vec.set v' col (Vec.dot v cols);
  ) col_range;
  v'

let mul_m2v m v =
  let size = Mat.row_size m in
  let col_size = Mat.col_size m in
  let v' = Vec.make size 0.0 in
  let row_range = Mat.row_size m |> Size.to_int |> Util.range in
  List.iter (fun row ->
    let rows = list_to_vec (Mat.row_of_mat ~row m) col_size in
    Vec.set v' row (Vec.dot v rows);
  ) row_range;
  v'

module Open = struct
  let ( *> ) = mul_v2m
  let ( *< ) = mul_m2v
end

(* Jacobi反復法による、係数行列を利用した方程式の数値解法を行う。 *)
let jacobi ?(epsilon=0.0000001) ~coefficient:coef ~const:v () =

  let get_solution coef index v k =
  (* TODO:対角行列における、indexに対応する対角値 *)
    let diagonal = Mat.get ~row:index ~col:index coef |> Util.some in
  (* TODO: 対象の定数 *)
    let b = Vec.get ~index v |> Util.some in
  (* TODO: 計算対象のインデックスを一覧する。ただし、今回対象のインデックスは見ないようにする *)
    let col_count = Mat.col_size coef |> Size.to_int |> Util.range |> List.filter ((<>) index) in
    let sum = List.fold_left (fun s col ->
    (* TODO: 係数行列における、対象行の値 *)
      let aij = Mat.get ~row:index ~col coef |> Util.some in
    (* TODO: 前回の計算結果 *)
      let xj = Vec.get ~index:col k |> Util.some in
      s +. (aij /. diagonal) *. xj)
      0.0 col_count
    in
    b /. diagonal -. sum
  in

  let is_allowable epsilon k k' =
    let len = Vec.size k |> Size.to_int |> Util.range in
    let diff = List.fold_left (fun sum index ->
      let k = Vec.get ~index k |> Util.some 
      and k' = Vec.get ~index k' |> Util.some in
      sum +. (abs_float (k' -. k))
    ) 0.0 len in
    diff <= epsilon
  in

  let sol_size = Vec.size v in
  let sol_count = Size.to_int sol_size |> Util.range in
  (* TODO:初期値となる、適当な未知数のベクトルを用意する *)
  let ret = Vec.make sol_size 1.0 in
  (* TODO: k + 1に対応する未知数を計算していく。計算結果が許容範囲になった段階で抜ける *)
  let rec jacobi' k =
    let k' = Vec.make sol_size 0.0 in
    (* kの各値から、k + 1 の値を算出する *)
    List.iter (fun index ->
      Vec.set ~index k' ~v:(get_solution coef index v k)
    ) sol_count;
    if is_allowable epsilon k k' then k'
    else jacobi' k'
  in
  jacobi' ret
