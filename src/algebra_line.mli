(** Provide functions for line. Detect line collision, Get nearest point on the line, and ohters. *)

type +'s p = 's Algebra.vec
(** The type of point contained a segment. *)

type +'s t = 's p * 's p
(** The type of segment. Constructed start and end of the segment. *)

type 's detect_type =
  | Collide  of 's t
  | Nearest  of 's t
  | Parallel  (** The type of detect the point for between two line. *)

val detect_collision : ?epsilon:Algebra_types.num_type -> a:'s t -> b:'s t -> unit -> 's t option
(** [detect_collision ~a ~b ()] get the result of detection to be collided [a] and [b]. If not collide [a] and [b], get
    [None] caller. *)

val nearest_point : ?epsilon:Algebra_types.num_type -> a:'s t -> b:'s t -> unit -> 's detect_type
(** [nearest_point ~a ~b ()] get the nearest points between [a] and [b]. If [a] and [b] is parallel, can not return any
    point, so return [Parallel]. *)
