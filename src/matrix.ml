module S = Size
module V = Vector

include Matrix_intf

module Make(T:TYPE):S with type num_type := T.num_type = struct

  type num_type = T.num_type
  type (+'row, 'col, 'd) t = {
    data: num_type array array;
    row_size: 'row;
    col_size: 'col;
  }

  let init ~row ~col ~f =
    let col' = S.to_int col
    and row' = S.to_int row in
    if col' < 1 || row' < 1 then raise (Invalid_argument ("Matrix size must be greater than 1"))
    else
      let cols = Array.init col' (fun _ -> f 0 0) in
      let mat = Array.init row' (fun _ -> Array.copy cols) in
      let col_range = Util.range col'
      and row_range = Util.range row' in
      List.iter (fun r -> List.iter
        (fun c ->
          mat.(r).(c) <- f r c
        ) col_range
      ) row_range;

      {data = mat; row_size = row; col_size = col;}

  let make ~row ~col ~init:v = init ~row ~col ~f:(fun _ _ -> v)

  let row_size {row_size;_} = row_size
  let col_size {col_size;_} = col_size

  let get ~row ~col mat =
    if Array.length mat.data <= row then None
    else if Array.length mat.data.(row) <= col then None
    else Some mat.data.(row).(col)

  let unsafe_get ~row ~col mat =
    match get ~row ~col mat with
    | None -> failwith "Invalid boundary of matrix"
    | Some v -> v

  let set ~row ~col ~v mat =
    if Array.length mat.data <= row then failwith "Invalid row"
    else if Array.length mat.data.(row) <= col then failwith "Invalid column"
    else mat.data.(row).(col) <- v

  let transpose mat =
    make ~row:mat.col_size ~col:mat.row_size ~init:(unsafe_get ~row:0 ~col:0 mat)
end
