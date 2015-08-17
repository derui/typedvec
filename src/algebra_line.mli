(**
   Provide functions for line. Detect line collision, Get nearest point on the line, and ohters.
*)

type +'s p = 's Algebra.vec
(* The type of point contained a segment. *)
type +'s t = 's p * 's p
(** The type of segment. Constructed start and end of the segment. *)

val detect_collision: ?epsilon:Algebra_types.num_type -> a:'s t -> b:'s t -> 's p option
(* [detect_collision ~a ~b] get the result of detection to be collided [a] and [b].
   If not collide [a] and [b], get [None] caller.
*)
