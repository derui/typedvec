let id e = e
let flip f a b = f b a

let range n =
  let rec range' n list =
    if n = 0 then list
    else let n' = pred n in range' n' (n' :: list)
  in
  if n < 0 then [] else range' n []

(* [separate idx ary] get to take 0 to [idx] from [ary], and take [idx] + 1 to
   length of [ary].*)
let separate index ary =
  let len = Array.length ary in
  let index = max 0 index in
  let tak = Array.sub ary 0 index
  and lst = Array.sub ary (succ index) (len - (succ index)) in
  (tak, lst)

let some = function
  | None -> failwith "[some] is able to apply only Some."
  | Some s -> s
