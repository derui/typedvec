(* The Algebraic module via float. *)
type num_type = float

module Mat : sig
  include Matrix_intf.S with type num_type := num_type
  (* # Operations for Matrix. *)

  type 'a s = 'a Size.t

  val identity: 'a s -> ('a s, 'a s, num_type) t
  (* [identity size] get the identity matrix having [size] rows and [size] columns. *)

  val scalar: m:('a s, 'b s, num_type) t -> scale:num_type -> ('a s, 'b s, num_type) t
  (* [scalar ~m ~scale] get the scaled matrix that multiply scale with each elements of [m] *)

  val diagonal: size:'a s -> comp:num_type -> ('a s, 'a s, num_type) t
  (* [diagonal ~size ~comp] get the diagonal matrix having [size] rows and columns, and each component
     is [comp]. *)

  val add: ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
  (* [add a b] get result to add [b] to [a]. *)

  val sub: ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
  (* [sub a b] get result to subtract [b] from [a]. *)

  val mul: ('a s, 'b s, num_type) t -> ('b s, 'c s, num_type) t -> ('a s, 'c s, num_type) t
  (* [mul a b] get result to multiply [a] with [b]. *)

  val det: ('a s, 'a s, num_type)t -> num_type option
  (* [det mat] get the determinant of the [mat] if it have. *)

  val inverse: ('a s, 'a s, num_type) t -> ('a s, 'a s, num_type) t option
(* [inverse mat] get the inverse matrix of [matrix]. If [matrix] not have the inverse matrix,
   return None.
   [mat] must be a square matrix.*)

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

  val scalar: scale:num_type -> v:('s, num_type) t -> ('s, num_type) t
(* [scaler ~scale ~v] multiply [scale] with each elements of [v]. *)
end

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

