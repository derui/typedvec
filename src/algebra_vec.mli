type num_type = Algebra_types.num_type
(** Provide functions for Vector. Basic four operations for arithmetic, and operator it, and others. *)

include Vector_intf.S with type num_type := num_type

(** [Open] module provide shortcut operators are defined in [Vec] module. *)
module Open : sig
  val ( +: ) : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t

  val ( -: ) : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t

  val ( *: ) : ('a, num_type) t -> ('a, num_type) t -> num_type

  val ( /: ) : ('a, num_type) t -> num_type -> ('a, num_type) t
end

(* # Operations for Vector. *)

val add : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
(** [add a b] get the new vector that append [a] to [b]. *)

val sub : ('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t

(** [sub a b] get the new vector that subtract [b] from [a]. *)

val dot : ('a, num_type) t -> ('a, num_type) t -> num_type
(** [dot a b] get the new vector that dot product via [a] and [b]. *)

val div : ('a, num_type) t -> num_type -> ('a, num_type) t
(** [div a divisor] get the divided [a] with [divisor]. Notice vector getting from this is new one from [a] *)

val zero : 'a Size.t -> ('a Size.t, num_type) t
(** [zero] get the zero vector specified size. *)

val normalize : ('a, num_type) t -> ('a, num_type) t
(** [normalize v] get the normalized [v]. *)

val norm_sqrt : ('a, num_type) t -> num_type
(** [norm_sqrt v] get the norm of the [v]. *)

val norm : ('a, num_type) t -> num_type
(** [norm v] get the norm of the [v]. The difference from [norm_sqrt] is not apply square-root of a norm of [v]. *)

val cross :
  left:(Size.three Size.t, num_type) t -> right:(Size.three Size.t, num_type) t -> (Size.three Size.t, num_type) t
(** [cross ~left ~right] gets new vector is cross product via 2 vectors. *)

val scalar : scale:num_type -> ('s, num_type) t -> ('s, num_type) t
(** [scaler ~scale ~v] multiply [scale] with each elements of [v]. *)

val inverse : ('s, num_type) t -> ('s, num_type) t
(** [inverse v] get the vertex inverted. This function is equals to call [scalar ~scale:-1.0 ~v]. *)
