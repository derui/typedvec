module T = Typedvec

let%spec "Util.range should be able to make integer list" =
  let open T.Util in 
  (range (-1)) [@eq []];
  (range 0) [@eq []];
  (range 1) [@eq [0]];
  (range 2) [@eq [0;1]]

