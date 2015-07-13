module S = Size
type num_type' = float
type num_type = num_type'

module V = Vector.Make(struct
  type num_type = num_type'
end)

module Vec = struct
  include V

  let add l r = V.map2 ~f:(fun l r -> l +. r) ~v1:l ~v2:r
  let sub l r = V.map2 ~f:(fun l r -> l -. r) ~v1:l ~v2:r
  let dot l r = V.map2 ~f:( *. ) ~v1:l ~v2:r |> V.fold_left ~f:(+.) ~init:0.0
  let div l d = V.map ~f:(fun n -> n /. d) l

  let cross ~left ~right =
    let v = V.make Size.three 0.0 in
    let g = V.unsafe_get in
    V.set v 0 ((g left 1) *. (g right 2) -. (g left 2) *. (g right 1));
    V.set v 1 ((g left 2) *. (g right 0) -. (g left 0) *. (g right 2));
    V.set v 2 ((g left 0) *. (g right 1) -. (g left 1) *. (g right 0));
    v

  let scalar ~scale ~v  = V.map ~f:(fun v -> v *. scale) v

  let norm v =
    let v = V.to_list v in
    List.fold_left (fun s v -> s +. v *. v) 0.0 v

  let norm_sqrt v = norm v |> sqrt

  module Open = struct
    let (+:) = add
    let (-:) = sub
    let ( *: ) = dot
    let (/:) = div
  end
end

module Mat = struct
  include Matrix.Make(struct
    type num_type = num_type'
  end)

  type 'a s = 'a Size.t

  let identity size =
    let s = Size.to_int size in
    if s <= 0 then failwith "Size of identity Matrix must greater equal 1."
    else let m = make ~row:size ~col:size ~init:0.0 in
         Util.range s |> List.iter (fun i -> set ~row:i ~col:i ~v:1.0 m);
         m

  let scalar ~m ~scale = map ~f:(fun _ _ v -> v *. scale) m
  let diagonal ~size ~comp =
    let m = identity size in
    scalar ~m ~scale:comp

  let apply_each_element f = function
    | (None, _) | (_, None) -> failwith "Two matrix must have equality row and col"
    | (Some a, Some b) -> f a b

  let add a b = map ~f:(fun row col _ ->
    apply_each_element (+.) ((get ~row ~col a), (get ~row ~col b))) a
  let sub a b = map ~f:(fun row col _ ->
    apply_each_element (-.) ((get ~row ~col a), (get ~row ~col b))) a

  let mul a b =
    let mat = make ~row:(row_size a) ~col:(col_size b) ~init:0.0 in
    let rows = Util.range (row_size a |> Size.to_int)
    and cols = Util.range (col_size b |> Size.to_int) in
    List.iter (fun row ->
      let rl = row_of_mat ~row a in
      List.iter (fun col ->
        let cl = col_of_mat ~col b in
        let v = List.fold_left2 (fun sum a b -> sum +. (a *. b)) 0.0 rl cl in
        set ~row ~col ~v mat
      ) rows
    ) cols;
    mat

  (* TODO: 指定したcol/rowの余因子を返す *)
  let cofactor ~col ~row mat =
    if Array.length mat = 1 then mat
    else
      let (first, second) = Util.separate row mat in
      let concat_tuple ary = let (f,s) = Util.separate col ary in Array.concat [f;s] in
      let first = Array.map concat_tuple first
      and second = Array.map concat_tuple second in
      Array.concat [first;second]

  let factor_sign ~row ~col = -1.0 ** ((succ row |> float_of_int) +. (succ col |> float_of_int))

  let rec inner_det mat =
    if Array.length mat = 1 then mat.(0).(0)
    else
        (* TODO とりあえず一列目に対して余因子行列を取得し、その行列式を求める *)
      let factors = Util.range (Array.length mat) in
      let factors = List.map (fun row -> (row, cofactor ~col:0 ~row mat |> inner_det)) factors in
        (* TODO 求めた行列式から、余因子を求める *)
      let factors = List.map (fun (row, det_cof) -> mat.(row).(0) *. det_cof *.
        (factor_sign ~row ~col:0)) factors in
        (* 求めた余因子を全て加算する。 *)
      List.fold_left (+.) 0.0 factors

  let det mat =
    let ret = to_array mat |> inner_det in
    if ret <> 0.0 then Some ret else None

  (* TODO: 指定した正方行列に対する余因子行列を返す *)
  let adjugate mat =
    let row = row_size mat 
    and col = col_size mat in
    let mat' = to_array mat in
    let adj = init ~row ~col ~f:(fun row col ->
        let cof_det = cofactor ~row ~col mat' |> inner_det in
        (factor_sign ~row ~col) *. cof_det
    ) in
    transpose adj

  let inverse mat =
    match det mat with
    | None -> None
    | Some det ->
       let adj = adjugate mat in
       Some (scalar ~scale:(1.0 /. det) ~m:adj)

  module Open = struct
    let (+:) = add
    let (-:) = sub
    let ( *: ) = mul
  end
end

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
  let ret = V.make sol_size 1.0 in
  (* TODO: k + 1に対応する未知数を計算していく。計算結果が許容範囲になった段階で抜ける *)
  let rec jacobi' k =
    let k' = V.make sol_size 0.0 in
    (* kの各値から、k + 1 の値を算出する *)
    List.iter (fun index ->
      V.set ~index k' ~v:(get_solution coef index v k)
    ) sol_count;
    if is_allowable epsilon k k' then k'
    else jacobi' k'
  in
  jacobi' ret
