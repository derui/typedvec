include Matrix_intf.S with type num_type := Algebra_types.num_type
(** Provide functions for Matrix. Basic operations for matrix, and some solutions for using matrix, and others. *)

(* # Operations for Matrix. *)

type num_type = Algebra_types.num_type

type 'a s = 'a Size.t

(** [Open] module provide shortcut operators are defined in [Mat] module. *)
module Open : sig
  val ( +: ) : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t

  val ( -: ) : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t

  val ( *: ) : ('a s, 'b s, num_type) t -> ('b s, 'c s, num_type) t -> ('a s, 'c s, num_type) t
end

val identity : 'a s -> ('a s, 'a s, num_type) t
(** [identity size] get the identity matrix having [size] rows and [size] columns. *)

val scalar : scale:num_type -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
(** [scalar ~m ~scale] get the scaled matrix that multiply scale with each elements of [m] *)

val diagonal : comp:num_type -> 'a s -> ('a s, 'a s, num_type) t
(** [diagonal ~size ~comp] get the diagonal matrix having [size] rows and columns, and each component is [comp]. The
    diagonal matrix can make with functions defined as [scalar] and [identity] in this module. *)

val add : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
(** [add a b] get result to add [b] to [a]. *)

val sub : ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t -> ('a s, 'b s, num_type) t
(** [sub a b] get result to subtract [b] from [a]. *)

val mul : ('a s, 'b s, num_type) t -> ('b s, 'c s, num_type) t -> ('a s, 'c s, num_type) t
(** [mul a b] get result to multiply [a] with [b]. *)

val det : ('a s, 'a s, num_type) t -> num_type option
(** [det mat] get the determinant of the [mat] if it have. *)

val inverse : ('a s, 'a s, num_type) t -> ('a s, 'a s, num_type) t option
(** [inverse mat] get the inverse matrix of [matrix]. If [matrix] not have the inverse matrix, return None. [mat] must
    be a square matrix.*)
