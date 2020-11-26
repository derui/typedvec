open Vector_intf

(** The functor to make Vector module with element type. *)
module Make (T : TYPE) : S with type num_type := T.num_type
