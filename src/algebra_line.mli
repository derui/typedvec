(**
   Provide functions for line. Detect line collision, Get nearest point on the line, and ohters.
*)

type +'s t = 's Algebra.vec * 's Algebra.vec
(** The type of segment. Constructed start and end of the segment. *)

val detect_collision: a:'s t -> b:'s t -> 's t option
(* [detect_collision ~a ~b] get the result of detection to be collided [a] and [b].
   If not collide [a] and [b], get [None] caller.
*)
