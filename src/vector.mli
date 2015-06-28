
open Vector_intf

module Make(T:TYPE) : S with type num_type := T.num_type
(** The functor to make Vector module with element type. *)
