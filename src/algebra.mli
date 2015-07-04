(* The Algebraic module via float. *)
type num_type = float

module Mat : sig
  include Matrix_intf.S with type num_type := num_type
end

module Vec : sig
  include Vector_intf.S with type num_type := num_type


  (* # Operations for Vector. *)

  val add : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
  (* [add a b] get the new vector that append [a] to [b]. *)

  val sub : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
  (* [sub a b] get the new vector that subtract [b] from [a]. *)

  val dot : ('a, num_type) t -> ('a, num_type) t -> num_type
  (* [dot a b] get the new vector that dot product via [a] and [b]. *)

  val div : ('a, num_type) t -> num_type -> ('a, num_type) t
(* [div a divisor] get the divided [a] with [divisor]. Notice vector getting from this is new
   one from [a]
*)

  val cross: left:(Size.three Size.t, num_type) t -> right:(Size.three Size.t, num_type) t
    -> (Size.three Size.t, num_type) t
(** [cross ~left ~right] gets new vector is cross product via 2 vectors. *)
end

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

