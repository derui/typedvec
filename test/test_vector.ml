module Std = Typedvec.Std
module S = Std.Size
module V = Std.Vec.Make(struct
  type num_type = int
  let compare = Pervasives.compare
end)

let%spec "Make array from size" =
  let v = V.make S.two 0 in
  (V.size v |> S.to_int) [@eq 2];
  ([V.unsafe_get v 0; V.unsafe_get v 1]) [@eq [0;0]]

let%spec "Vec should raise Invalid_argument when make with given size less than 1" =
  (fun () -> V.make S.zero 0) [@raises Invalid_argument "Vector size must be greater than 1"]

let%spec "Vec should raise Invalid_argument when init with given size less than 1" =
  (fun () -> V.init S.zero (fun _ -> 0)) [@raises Invalid_argument "Vector size must be greater than 1"]

let%spec "Vec can manipulate each element" =
  let v = V.make S.two 0 in
  V.unsafe_set v 0 100;
  (V.unsafe_get v 0) [@eq 100]

let%spec "Vec can manipulate each element with safe function" =
  let v = V.make S.two 0 in
  V.set v 0 100;
  V.get v 0 [@eq (Some 100)];
  V.set v (-1) 10;
  V.get v (-1) [@eq None];
  V.set v 2 20;
  V.get v 2 [@eq None]

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
  V.for_all ~f:(fun e -> e > 0) v [@true];
  V.for_all ~f:(fun e -> e > 1) v [@false];
  let v = V.make S.one 0 in
  V.for_all ~f:(fun e -> e > 0) v [@false]

let%spec "Vec can detect to exist element that is predicated or not" =
  let v = V.init S.three (fun i -> succ i) in
  V.exists ~f:(fun e -> e > 0) v [@true];
  V.exists ~f:(fun e -> e = 3) v [@true];
  V.exists ~f:(fun e -> e < 1) v [@false];
  let v = V.make S.one 0 in
  V.exists ~f:(fun e -> e = 0) v [@true]

let%spec "Vec can be contained member in the vector" =
  let v = V.init S.four (fun i -> succ i) in
  V.mem ~member:1 v [@true];
  V.mem ~member:0 v [@false];
  let module V = Std.Vec.Make(struct
    type num_type = int list
    let compare = Pervasives.compare
  end) in
  let v = V.init S.four (fun i -> [i]) in
  V.memq ~member:[1] v [@false]

let%spec "Vec can do map iteration with 2 vectors" =
  let v1 = V.init S.three (fun i -> succ i)
  and v2 = V.init S.three (fun i -> pred i) in
  let mapped = V.map2 ~f:(fun a b -> a * b) ~v1 ~v2 in
  V.to_list mapped [@eq [-1;0;3]]

let%spec "Vec can do iteration with 2 vectors" =
  let v1 = V.init S.three (fun i -> succ i)
  and v2 = V.init S.three (fun i -> i + 2) in
  let r = ref 0 in
  V.iter2 ~f:(fun a b -> r := !r + (a * b)) ~v1 ~v2;
  !r [@eq 20]

let%spec "Vec can zip 2 vectors to one vector" =
  let v1 = V.init S.three (fun i -> succ i)
  and v2 = V.init S.three (fun i -> i + 2) in
  let zipped = V.zip ~v1 ~v2 in
  V.to_list zipped [@eq [(1,2);(2,3);(3,4)]]


let%spec "Vec can copy to new allocated vector" =
  let v = V.init S.three (fun i -> succ i) in
  (V.copy v |> V.to_list) [@eq [1;2;3]]

let %spec "Vec can copy to already allocated vector" =
  let v = V.init S.three (fun i -> succ i)
  and v2 = V.make S.three (-1) in
  V.copy ~y:v2 v |> ignore;
  V.to_list v2 [@eq [1;2;3]]

let%spec "Vec can detect equality vectors" =
  let v = V.init S.three (fun i -> succ i)
  and v2 = V.init S.three (fun i -> succ i)
  and v3 = V.make S.three 1 in

  V.equals v v2 [@true];
  V.equals v2 v [@true];
  V.equals v v3 [@false];
  V.equals v2 v3 [@false]
