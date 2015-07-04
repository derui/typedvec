let id e = e
let flip f a b = f b a

let range n =
  let rec range' n list =
    if n = 0 then list
    else let n' = pred n in range' n' (n' :: list)
  in
  if n < 0 then [] else range' n []
