type 'a t = int

(* These types are only used to as phantom type. *)
type 'a s

type 'a p

type z

let succ = Stdlib.succ

let pred = Stdlib.pred

type zero = z

type one = z s

type two = z s s

type three = z s s s

type four = z s s s s

type five = z s s s s s

type six = z s s s s s s

type seven = z s s s s s s s

type eight = z s s s s s s s s

type nine = z s s s s s s s s s

type ten = z s s s s s s s s s s

let zero = 0

let one = succ zero

let two = succ one

let three = succ two

let four = succ three

let five = succ four

let six = succ five

let seven = succ six

let eight = succ seven

let nine = succ eight

let ten = succ nine

let to_int n = n

module type F = sig
  type size

  val size : size t
end

module type SIZE = sig
  type size

  val size : size t

  val to_int : int
end

module S (F : F) : SIZE with type size = F.size = struct
  type size = F.size

  let size : size t = F.size

  let to_int = F.size
end

let of_int : int -> (module SIZE) =
 fun n ->
  let rec fold' n (module F : F) =
    if n <= 0 then (module S (F) : SIZE)
    else
      fold' (pred n)
        ( module struct
          type size = F.size s

          let size = succ F.size
        end : F )
  in
  fold' n
    ( module struct
      type size = z

      let size = 0
    end : F )

let iszero n = n = 0
