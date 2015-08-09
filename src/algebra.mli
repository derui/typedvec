(* The Algebraic module via float. *)
type num_type = float

module Mat : sig
  include Matrix_intf.S with type num_type := num_type
  (* # Operations for Matrix. *)

  type 'a s = 'a Size.t

  (* [Open] module provide shortcut operators are defined in [Mat] module. *)
  module Open : sig
    val (+:) : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
    val (-:) : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
    val ( *: ) : ('a s, 'b s, num_type) t -> ('b s, 'c s, num_type) t -> ('a s, 'c s, num_type) t
  end

  val identity: 'a s -> ('a s, 'a s, num_type) t
  (* [identity size] get the identity matrix having [size] rows and [size] columns. *)

  val scalar: m:('a s, 'b s, num_type) t -> scale:num_type -> ('a s, 'b s, num_type) t
  (* [scalar ~m ~scale] get the scaled matrix that multiply scale with each elements of [m] *)

  val diagonal: size:'a s -> comp:num_type -> ('a s, 'a s, num_type) t
  (* [diagonal ~size ~comp] get the diagonal matrix having [size] rows and columns, and each component
     is [comp].
     The diagonal matrix can make with functions defined as [scalar] and [identity] in this module.
  *)

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

  (* [Open] module provide shortcut operators are defined in [Vec] module. *)
  module Open : sig
    val (+:) : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
    val (-:) : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
    val ( *: ) : ('a, num_type) t -> ('a, num_type) t -> num_type
    val (/:) : ('a, num_type) t -> num_type -> ('a, num_type) t
  end

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

  val zero: 'a Size.t -> ('a Size.t, num_type) t
  (* [zero] get the zero vector specified size. *)

  val normalize: ('a, num_type) t -> ('a, num_type) t
  (* [normalize v] get the normalized [v]. *)

  val norm_sqrt: ('a, num_type) t -> num_type
  (* [norm_sqrt v] get the norm of the [v]. *)

  val norm: ('a, num_type) t -> num_type
  (* [norm v] get the norm of the [v]. The difference from [norm_sqrt] is
     not apply square-root of a norm of [v].
  *)

  val cross: left:(Size.three Size.t, num_type) t -> right:(Size.three Size.t, num_type) t
    -> (Size.three Size.t, num_type) t
  (** [cross ~left ~right] gets new vector is cross product via 2 vectors. *)

  val scalar: scale:num_type -> v:('s, num_type) t -> ('s, num_type) t
(* [scaler ~scale ~v] multiply [scale] with each elements of [v]. *)

  val inverse: ('s, num_type) t -> ('s, num_type) t
(* [inverse v] get the vertex inverted. This function is equals to call [scalar ~scale:-1.0 ~v]. *)
end

type (+'row, +'col) mat = ('row, 'col, num_type) Mat.t
(* The type of matrix using in this module. *)

type +'s vec = ('s, num_type) Vec.t
(* The type of vector using in this module. *)

type 'a s = 'a Size.t

(* This module provides shortcut operators for oerations to multiply matrix and vector. *)
module Open : sig
  val ( *> ): 's s vec -> ('s s, 'b s) mat -> 'b s vec
  val ( *< ): ('b s, 's s) mat -> 's s vec -> 'b s vec
end

val mul_v2m: 's s vec -> ('s s, 'b s) mat -> 'b s vec
(* [mul_v2m vec mat] multiply mat with vector. The [vec] is as "column" vector. *)

val mul_m2v: ('b s, 's s) mat -> 's s vec -> 'b s vec
(* [mul_m2v mat vec] multiply [vec] with [mat]. The [vec] is as "row" vector. *)

(* # Solutions *)

val jacobi: ?epsilon:num_type -> coefficient:('a s, 'a s) mat -> const:'a s vec -> unit -> 'a s vec
(* [jacobi ~coefficient ~const] get the value of unknown quantity in an equation using with Jacobi method. *)
