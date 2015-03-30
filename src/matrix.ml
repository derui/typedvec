module S = Size 
module V = Vector 

type (+'row, 'col, 'd) t = {
  data: ('row, ('col, 'd) Vector.t) Vector.t;
  row_size: 'row;
  col_size: 'col;
}

let init ~row ~col ~f =
  let col' = S.to_int col
  and row' = S.to_int row in
  if col' < 1 || row' < 1 then raise (Invalid_argument ("Matrix size must be greater than 1"))
  else
    let cols = Vector.make col (f 0 0) in
    let mat = Vector.init row (fun _ -> V.copy cols) in
    let col_range = Util.range col'
    and row_range = Util.range row' in
    List.iter (fun r -> List.iter
      (fun c ->
        let row = V.unsafe_get mat r in V.set row c (f r c)
      ) col_range
    ) row_range;

    {data = mat;
     row_size = row;
     col_size = col;
    }

let make ~row ~col ~init:v = init ~row ~col ~f:(fun _ _ -> v)


let row_size {row_size;_} = row_size
let col_size {col_size;_} = col_size

let get_row ~row mat = Vector.get mat.data row
let get ~row ~col mat =
  let row = Vector.get mat.data row in
  match row with
  | None -> None
  | Some row -> Vector.get row col

let unsafe_get ~row ~col mat =
  match get ~row ~col mat with
  | None -> failwith "Invalid boundary of matrix"
  | Some v -> v

let transpose: ('a Size.t, 'b Size.t, 'c) t -> ('b Size.t, 'a Size.t, 'c) t = fun mat ->
  let mat' = make ~row:mat.col_size ~col:mat.row_size ~init:(unsafe_get ~row:0 ~col:0 mat) in
  mat'
