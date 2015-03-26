type (+'n, 'b) t = {
  data : 'b array;
  size: 'n;
}

let make size init =
  let size' = Size.to_int size in
  {data = Array.make size' init; size}

let init size f =
  let size' = Size.to_int size in
  {data = Array.init size' f; size}

let size {size;_} = size

let get v i = v.data.(i)
let set v i newv = v.data.(i) <- newv

let to_list {data;_} = Array.to_list data

let map ~f v = {v with data = Array.map f v.data}
let iter ~f v = Array.iter f v.data
let mapi ~f v = {v with data = Array.mapi f v.data}
let iteri ~f v = Array.iteri f v.data

let fold_left ~f ~init v = Array.fold_left f init v.data
let fold_right ~f ~init v = Array.fold_right f v.data init

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
