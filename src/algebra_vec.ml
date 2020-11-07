module S = Size

type num_type' = float

type num_type = num_type'

module V = Vector.Make (struct
  type num_type = num_type'

  let compare = Float.compare
end)

include V

let add l r = V.map2 ~f:(fun l r -> l +. r) ~v1:l ~v2:r

let sub l r = V.map2 ~f:(fun l r -> l -. r) ~v1:l ~v2:r

let dot l r = V.map2 ~f:( *. ) ~v1:l ~v2:r |> V.fold_left ~f:( +. ) ~init:0.0

let div l d = V.map ~f:(fun n -> n /. d) l

let cross ~left ~right =
  let v = V.make Size.three 0.0 in
  let g = V.unsafe_get in
  V.set v ~index:0 ~v:((g left 1 *. g right 2) -. (g left 2 *. g right 1));
  V.set v ~index:1 ~v:((g left 2 *. g right 0) -. (g left 0 *. g right 2));
  V.set v ~index:2 ~v:((g left 0 *. g right 1) -. (g left 1 *. g right 0));
  v

let scalar ~scale v = V.map ~f:(fun v -> v *. scale) v

let zero size = V.make size 0.0

let norm v =
  let v = V.to_list v in
  List.fold_left (fun s v -> s +. (v *. v)) 0.0 v

let norm_sqrt v = norm v |> sqrt

let normalize v =
  let norm = norm_sqrt v in
  V.map ~f:(fun e -> e /. norm) v

let inverse v = scalar ~scale:(-1.0) v

module Open = struct
  let ( +: ) = add

  let ( -: ) = sub

  let ( *: ) = dot

  let ( /: ) = div
end
