module S = Typedvec.Size
module V = Typedvec.Vec

let%spec "Make array from size" =
  let v = V.make S.two 0 in
  (V.size v |> S.to_int) [@eq 2];
  ([V.get v 0; V.get v 1]) [@eq [0;0]]

let%spec "Vec can manipulate each element" =
  let v = V.make S.two 0 in
  V.set v 0 100;
  (V.get v 0) [@eq 100]

let%spec "Vec can convert to list" =
  let v = V.init S.two (( * ) 2) in
  let ary = V.to_list v in
  ary [@eq [0;2]]

let%spec "Vec can be mapping with some function" =
  let v = V.init S.two (fun i -> succ i) in
  (V.map v ~f:(fun e -> succ e) |> V.to_list) [@eq [2;3]];
  (V.map v ~f:float_of_int |> V.to_list) [@eq [1.0;2.0]];
  (V.mapi v ~f:(fun i _ -> i) |> V.to_list) [@eq [0;1]]

let%spec "Vec can iterate with some function" =
  let v = V.init S.two (fun i -> succ i) in
  let r = ref 0 in
  V.iter v ~f:(fun e -> r := !r + e);
  !r [@eq 3];
  r := 0;
  V.iteri v ~f:(fun i _ -> r := !r + (pred i));
  !r [@eq -1]

let%spec "Vec can fold left to any vector" =
  let v = V.init S.three (fun i -> succ i) in
  (V.fold_left ~f:(fun s e -> s + e) ~init:0 v) [@eq 6];
  (V.fold_left ~f:(fun s e -> e :: s) ~init:[] v) [@eq [3;2;1]]

let%spec "Vec can fold right to any vector" =
  let v = V.init S.three (fun i -> succ i) in
  (V.fold_right ~f:(fun s e -> s + e) ~init:0 v) [@eq 6];
  (V.fold_right ~f:(fun e s -> e :: s) ~init:[] v) [@eq [1;2;3]]

let%spec "Vec can predicate for all element" =
  let v = V.init S.three (fun i -> succ i) in
  V.for_all ~f:(fun e -> e > 0) v [@eq true];
  V.for_all ~f:(fun e -> e > 1) v [@eq false];
  let v = V.make S.zero 0 in
  V.for_all ~f:(fun e -> e > 0) v [@eq true]

let%spec "Vec can detect to exist element that is predicated or not" =
  let v = V.init S.three (fun i -> succ i) in
  V.exists ~f:(fun e -> e > 0) v [@eq true];
  V.exists ~f:(fun e -> e = 3) v [@eq true];
  V.exists ~f:(fun e -> e < 1) v [@eq false];
  let v = V.make S.zero 0 in
  V.exists ~f:(fun e -> e = 0) v [@eq false]

let%spec "Vec can be contained member in the vector" =
  let v = V.init S.four (fun i -> succ i) in
  V.mem ~member:1 v [@eq true];
  V.mem ~member:0 v [@eq false];
  let v = V.init S.four (fun i -> [i]) in
  V.memq ~member:[1] v [@eq false]
