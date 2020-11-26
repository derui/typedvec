module S = Size
module V = Vector
include Matrix_intf

module Make (T : TYPE) : S with type num_type := T.num_type = struct
  type (+'row, 'col, 'd) t = {
    data : 'd array array;
    row_size : 'row;
    col_size : 'col;
  }

  let init ~row ~col ~f =
    let col' = S.to_int col and row' = S.to_int row in
    if col' < 1 || row' < 1 then raise (Invalid_argument "Matrix size must be greater than 1")
    else
      let cols = Array.init col' (fun _ -> f 0 0) in
      let mat = Array.init row' (fun _ -> Array.copy cols) in
      let col_range = Util.range col' and row_range = Util.range row' in
      List.iter (fun r -> List.iter (fun c -> mat.(r).(c) <- f r c) col_range) row_range;

      { data = mat; row_size = row; col_size = col }

  let make ~row ~col ~init:v = init ~row ~col ~f:(fun _ _ -> v)

  let row_size { row_size; _ } = row_size

  let col_size { col_size; _ } = col_size

  let get ~row ~col mat =
    if Array.length mat.data <= row then None
    else if Array.length mat.data.(row) <= col then None
    else Some mat.data.(row).(col)

  let unsafe_get ~row ~col mat =
    match get ~row ~col mat with None -> failwith "Invalid boundary of matrix" | Some v -> v

  let set ~row ~col ~v mat =
    if Array.length mat.data <= row then failwith "Invalid row"
    else if Array.length mat.data.(row) <= col then failwith "Invalid column"
    else mat.data.(row).(col) <- v

  let transpose mat = init ~row:mat.col_size ~col:mat.row_size ~f:(fun row col -> unsafe_get ~row:col ~col:row mat)

  let map ~f mat =
    let row = Size.to_int mat.row_size |> Util.range and col = Size.to_int mat.col_size |> Util.range in
    let fn row _ = List.mapi (fun col _ -> f row col mat.data.(row).(col)) col |> Array.of_list in
    let data = List.mapi fn row in
    { data = Array.of_list data; row_size = mat.row_size; col_size = mat.col_size }

  let row_of_mat ~row mat =
    if row < 0 || row_size mat |> Size.to_int < row then failwith "number of row must less than matrix"
    else Array.to_list mat.data.(row)

  let col_of_mat ~col mat =
    if col < 0 || col_size mat |> Size.to_int < col then failwith "number of col must less than matrix"
    else Array.map (fun ary -> ary.(col)) mat.data |> Array.to_list

  let to_array { data; _ } = data

  let to_list { data; _ } = Array.map Array.to_list data |> Array.to_list

  let copy mat = { mat with data = to_array mat }

  let submatrix ?(start_row = 0) ?(start_col = 0) ~row ~col m =
    let rs = Size.to_int row and cs = Size.to_int col in
    let mat_row = row_size m |> Size.to_int and mat_col = col_size m |> Size.to_int in
    if rs + start_row > mat_row || cs + start_col > mat_col then None
    else Some (init ~row ~col ~f:(fun r c -> unsafe_get ~row:(start_row + r) ~col:(start_col + c) m))
end
