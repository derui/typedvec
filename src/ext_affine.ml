open Core.Std
module A = Algebra
module Vec = Algebra_vec
module Mat = Algebra_mat

let translation_to_mat ?(dir=`Row) v =
  let s = Vec.size v in
  let size = Size.to_int s in
  let m = Mat.identity s in
  let range = List.range 0 size in
  begin
    match dir with
    | `Row -> begin
      List.iter range ~f:(fun index ->
        match Vec.get ~index v with
        | Some v -> Mat.set ~row:(size - 1) ~col:index ~v m
        | None -> ()
      )
    end
    | `Col -> begin
      List.iter range ~f:(fun index ->
        match Vec.get ~index v with
        | Some v -> Mat.set ~col:(size - 1) ~row:index ~v m
        | None -> ()
      )
    end
  end;
  m

let translation_of_mat ?(dir=`Row) m =
  let s = Mat.row_size m in
  let size = Size.to_int s in
  let v = Vec.zero s in
  let range = List.range 0 size in
  begin
    match dir with
    | `Row -> begin
      List.iter range ~f:(fun index ->
        match Mat.get ~row:(size - 1) ~col:index m with
        | Some v' -> Vec.set ~index ~v:v' v
        | None -> ()
      )
    end
    | `Col -> begin
      List.iter range ~f:(fun index ->
        match Mat.get ~col:(size - 1) ~row:index m with
        | Some v' -> Vec.set ~index ~v:v' v
        | None -> ()
      )
    end
  end;
  v
