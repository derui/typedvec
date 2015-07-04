module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module V = A.Vec

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
