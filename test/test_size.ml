[%%suite
module T = Typedvec

let%spec "Size to int" =
  let open T.Size in 
  (to_int one) [@eq 1];
  (to_int two) [@eq 2];
  (to_int three) [@eq 3];
  (to_int four) [@eq 4];
  (to_int five) [@eq 5];
  (to_int six) [@eq 6];
  (to_int seven) [@eq 7];
  (to_int eight) [@eq 8];
  (to_int nine) [@eq 9];
  (to_int ten) [@eq 10]

let%spec "Size can check zero or not" =
  let open T.Size in
  (iszero zero) [@true "zero equals to zero"];
  (iszero two) [@false "two not equals to zero"]

let%spec "Size can convert from int to type of Size" =
  let open T.Size in
  let module M = (val of_int 10) in
  (M.to_int) [@eq 10];
  (M.to_int = (to_int ten)) [@true]

]
