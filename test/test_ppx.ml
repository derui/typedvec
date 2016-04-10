[%%suite
 module Std = Typedvec.Std
 module S = Std.Size
 module V = Std.Algebra.Vec

 let%spec "Make vector with extension" =
   let v = [%vec [1.0;2.0]] in
   (V.size v |> S.to_int) [@eq 2];
   V.to_list v [@eq [1.0;2.0]]

 let%spec "Made vector with extension can compare via self make vector" =
   let v = [%vec [1.0;2.0]] in
   let v2 = V.init S.two (fun i -> succ i |> float_of_int) in
   V.equals v v2 [@true "Vectors not equals"]

 let%spec "Vector made extension can place in expression" =
   let v2 = V.init S.two (fun i -> succ i |> float_of_int) in
   V.equals [%vec [1.0;2.0]] v2 [@true "Vectors not equals"]

]
