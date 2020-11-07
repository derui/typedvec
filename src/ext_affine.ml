module A = Algebra
module Vec = Algebra_vec
module Mat = Algebra_mat

type 'a t = {
  size : 'a A.s;
  data : ('a Size.s A.s, 'a Size.s A.s) A.mat;
}

let make s = { data = Size.succ s |> Mat.identity; size = s }

let translate af ~vec =
  let size = Vec.size vec |> Size.to_int in
  let m = Mat.identity (Size.succ af.size) in
  Vec.iteri vec ~f:(fun index v -> Mat.set ~row:size ~col:index ~v m);
  let open Mat.Open in
  { af with data = af.data *: m }

let rotate af ~rotate =
  let m = Mat.identity (Size.succ af.size) in
  let rotate_size = Size.to_int af.size in
  let m =
    Mat.map m ~f:(fun row col v ->
        if row >= rotate_size || col >= rotate_size then v else Mat.get ~row ~col rotate |> Option.value ~default:v)
  in
  let open Mat.Open in
  { af with data = af.data *: m }

let scale af ~scale =
  let m = Mat.identity (Size.succ af.size) in
  Vec.iteri scale ~f:(fun index v -> Mat.set ~row:index ~col:index ~v m);
  let open Mat.Open in
  { af with data = af.data *: m }

let to_mat af = af.data

let of_mat ~size mat = { size; data = mat }
