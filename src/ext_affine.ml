open Core.Std
module A = Algebra
module Vec = Algebra_vec
module Mat = Algebra_mat

let translation ?(dir=`Row) v =
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
