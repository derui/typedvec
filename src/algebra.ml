module S = Size
type num_type' = float
type num_type = num_type'

module Mat = Matrix.Make(struct
  type num_type = num_type'
end)

module Vec = Vector.Make(struct
  type num_type = num_type'
end)

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

let cross ~left ~right =
  let v = Vec.make Size.three 0.0 in
  let g = Vec.unsafe_get in
  Vec.set v 0 ((g left 1) *. (g right 2) -. (g left 2) *. (g right 1));
  Vec.set v 1 ((g left 2) *. (g right 0) -. (g left 0) *. (g right 2));
  Vec.set v 2 ((g left 0) *. (g right 1) -. (g left 1) *. (g right 0));
  v
