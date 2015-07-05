module S = Size
type num_type' = float
type num_type = num_type'

module V = Vector.Make(struct
  type num_type = num_type'
end)

module Vec = struct
  include V

  let add l r = V.map2 ~f:(fun l r -> l +. r) ~v1:l ~v2:r
  let sub l r = V.map2 ~f:(fun l r -> l -. r) ~v1:l ~v2:r
  let dot l r = V.map2 ~f:( *. ) ~v1:l ~v2:r |> V.fold_left ~f:(+.) ~init:0.0
  let div l d = V.map ~f:(fun n -> n /. d) l

  let cross ~left ~right =
    let v = V.make Size.three 0.0 in
    let g = V.unsafe_get in
    V.set v 0 ((g left 1) *. (g right 2) -. (g left 2) *. (g right 1));
    V.set v 1 ((g left 2) *. (g right 0) -. (g left 0) *. (g right 2));
    V.set v 2 ((g left 0) *. (g right 1) -. (g left 1) *. (g right 0));
    v
end

module Mat = struct
  include Matrix.Make(struct
    type num_type = num_type'
  end)

  type 'a s = 'a Size.t

  let identity size =
    let s = Size.to_int size in
    if s <= 0 then failwith "Size of identity Matrix must greater equal 1."
    else let m = make ~row:size ~col:size ~init:0.0 in
         Util.range s |> List.iter (fun i -> set ~row:i ~col:i ~v:1.0 m);
         m

  let scalar ~m ~scale = map ~f:(fun _ _ v -> v *. scale) m
  let diagonal ~size ~comp =
    let m = identity size in
    scalar ~m ~scale:comp

  let apply_each_element f = function
    | (None, _) | (_, None) -> failwith "Two matrix must have equality row and col"
    | (Some a, Some b) -> f a b

  let add a b = map ~f:(fun row col _ ->
    apply_each_element (+.) ((get ~row ~col a), (get ~row ~col b))) a
  let sub a b = map ~f:(fun row col _ ->
    apply_each_element (-.) ((get ~row ~col a), (get ~row ~col b))) a

  let mul a b =
    let mat = make ~row:(row_size a) ~col:(col_size b) ~init:0.0 in
    let rows = Util.range (row_size a |> Size.to_int)
    and cols = Util.range (col_size b |> Size.to_int) in
    List.iter (fun row ->
      let rl = row_of_mat ~row a in
      List.iter (fun col ->
        let cl = col_of_mat ~col b in
        let v = List.fold_left2 (fun sum a b -> sum +. (a *. b)) 0.0 rl cl in
        set ~row ~col ~v mat
      ) rows
    ) cols;
    mat

end

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

