type (+'n, 'b) t = {
  data : 'b array;
  size: 'n;
}

let make size init =
  let size' = Size.to_int size in
  if size' < 1 then raise (Invalid_argument "Vector size must be greater than 1")
  else {data = Array.make size' init; size}

let init size f =
  let size' = Size.to_int size in
  if size' < 1 then raise (Invalid_argument "Vector size must be greater than 1")
  else {data = Array.init size' f; size}

let size {size;_} = size

let unsafe_get v i = v.data.(i)
let unsafe_set v i newv = v.data.(i) <- newv
let get v i = if i < 0 || i >= Array.length v.data then None else Some (unsafe_get v i)
let set v i newv = if i < 0 || i >= Array.length v.data then () else
    unsafe_set v i newv

let to_list {data;_} = Array.to_list data

let map ~f v = {v with data = Array.map f v.data}
let iter ~f v = Array.iter f v.data
let mapi ~f v = {v with data = Array.mapi f v.data}
let iteri ~f v = Array.iteri f v.data

let fold_left ~f ~init v = Array.fold_left f init v.data
let fold_right ~f ~init v = Array.fold_right f v.data init

let zip ~v1 ~v2 =
  let data = Array.mapi (fun index v -> (v, v2.data.(index))) v1.data in
  {data; size = v1.size}

let map2 ~f ~v1 ~v2 =
  let zipped = zip ~v1 ~v2 in
  map ~f:(fun (m1, m2) -> f m1 m2) zipped

let iter2 ~f ~v1 ~v2 =
  let zipped = zip ~v1 ~v2 in
  iter ~f:(fun (m1, m2) -> f m1 m2) zipped

let for_all ~f:pred v =
  let successes = fold_left ~f:(fun ret e ->
    if pred e then succ ret else ret
  ) ~init:0 v in
  successes = (Array.length v.data)

let exists ~f:pred v =
  let successes = fold_left ~f:(fun ret e ->
    if pred e then succ ret else ret
  ) ~init:0 v in
  successes > 0

let mem ~member v = exists ~f:(fun e -> member = e) v
let memq ~member v = exists ~f:(fun e -> member == e) v

let copy ?y v =
  match y with
  | None -> map ~f:Util.id v
  | Some y -> iteri ~f:(fun index m -> set y index m) v;
    v
