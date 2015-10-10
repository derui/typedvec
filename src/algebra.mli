(* The Algebraic module via float. *)
type num_type = float

type (+'row, +'col) mat = ('row, 'col, num_type) Algebra_mat.t
(* The type of matrix using in this module. *)

type +'s vec = ('s, num_type) Algebra_vec.t
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

val cross_to_mat: (Size.three s) vec -> (Size.three s, Size.three s) mat
(* [cross_to_mat vec] get the matrix to cross product any vector.
   Notion using matrix returned from this function only allows to apply [mat *< vec] only.
   [Vec.cross a b] equality [(cross_to_mat a) *< b].
*)

(* # Solutions *)

val jacobi: ?epsilon:num_type -> coefficient:('a s, 'a s) mat -> const:'a s vec -> unit -> 'a s vec
(* [jacobi ~coefficient ~const] get the value of unknown quantity in an equation using with Jacobi method. *)
