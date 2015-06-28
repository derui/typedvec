(* The Algebraic module via float. *)
type num_type = float

module Mat : sig
  include Matrix_intf.S with type num_type := num_type
end

module Vec : sig
  include Vector_intf.S with type num_type := num_type
end

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

val cross: left:Size.three Size.t vec -> right:Size.three Size.t vec -> Size.three Size.t vec
(** [cross ~left ~right] gets new vector is cross product via 2 vectors. *)
