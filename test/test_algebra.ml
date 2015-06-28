module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module V = A.Vec

let%spec "cross should return cross product with 2 vectors" =
  let a = V.make S.three 0.0
  and b = V.make S.three 0.0 in
  List.iteri (fun ind v -> V.set a ind v) [1.0;2.0;0.0];
  List.iteri (fun ind v -> V.set b ind v) [0.0;1.0;-1.0];
  let c = A.cross ~left:a ~right:b in
  V.to_list c [@eq [-2.0;1.0;1.0]]
