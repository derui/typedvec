[%%suite
 module Std = Typedvec.Std
 module AF = Typedvec.Ext.Affine
 module S = Std.Size
 module A = Std.Algebra
 module M = A.Mat

 let%spec "Affine should have identity matrix after immediately making" =
   let af = AF.make S.two in
   let m = A.Mat.identity S.three in
   (A.Mat.to_list (AF.to_mat af)) [@eq A.Mat.to_list m]

 let%spec "Affine can apply translation" =
   let af = AF.make S.two in
   let v = A.Vec.make S.two 2.0 in
   let af = AF.translate af ~vec:v in
   let m = AF.to_mat af in
   (M.get ~col:0 ~row:2 m) [@eq Some (2.0)];
   (M.get ~col:1 ~row:2 m) [@eq Some (2.0)];
   (M.get ~col:2 ~row:2 m) [@eq Some (1.0)]

 let%spec "Affine can apply multi translation" =
   let af = AF.make S.two in
   let v = A.Vec.make S.two 2.0 in
   let af = AF.translate af ~vec:v |> AF.translate ~vec:v in
   let m = AF.to_mat af in
   (M.get ~col:0 ~row:2 m) [@eq Some (4.0)];
   (M.get ~col:1 ~row:2 m) [@eq Some (4.0)];
   (M.get ~col:2 ~row:2 m) [@eq Some (1.0)]

 let%spec "Affine can apply scaling" =
   let af = AF.make S.two in
   let v = A.Vec.make S.two 2.0 in
   let af = AF.scale af ~scale:v in
   let m = AF.to_mat af in
   (M.get ~col:0 ~row:0 m) [@eq Some (2.0)];
   (M.get ~col:1 ~row:1 m) [@eq Some (2.0)];
   (M.get ~col:2 ~row:2 m) [@eq Some (1.0)]

]
