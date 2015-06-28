module A = Typedvec.Std.Algebra
module V = A.Vec

let%spec "cross should return cross product with 2 vectors" =
  let a = A.Vec.get
  A.cross 
