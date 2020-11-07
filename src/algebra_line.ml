module A = Algebra
module V = Algebra_vec

type +'s p = 's A.vec

type +'s t = 's p * 's p

type 's detect_type =
  | Collide  of 's t
  | Nearest  of 's t
  | Parallel

let nearest_point ?(epsilon = 0.00001) ~a ~b () =
  let open V.Open in
  let a' = snd a -: fst a and b' = snd b -: fst b in
  let na' = V.normalize a' and nb' = V.normalize b' in
  (* 線分同士の角度 *)
  let angle_norm = na' *: nb' in
  (* パラメトリックの値を取得するときに共通で利用するヘルパー *)
  let para_work = 1.0 -. (angle_norm *. angle_norm) in
  (* パラメトリックの値を取得するためのヘルパーベクトル *)
  match para_work = 0.0 with
  | true  -> Parallel
  | false ->
      let para_v = fst b -: fst a in
      (* aにおけるパラメトリックの値 *)
      let para_a = ((para_v *: na') -. (angle_norm *. (para_v *: nb'))) /. para_work in
      (* bにおけるパラメトリックの値 *)
      let para_b = ((angle_norm *. (para_v *: na')) -. (para_v *: nb')) /. para_work in
      let para_a' = V.scalar ~scale:para_a na' and para_b' = V.scalar ~scale:para_b nb' in
      let p_a = fst a +: para_a' and p_b = fst b +: para_b' in
      if V.norm (p_a -: p_b) < epsilon then Collide (p_a, p_b) else Nearest (p_a, p_b)

let detect_collision ?(epsilon = 0.00001) ~a ~b () =
  match nearest_point ~epsilon ~a ~b () with Collide p -> Some p | _ -> None
