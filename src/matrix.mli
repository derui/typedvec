open Matrix_intf

module Make (T : TYPE) : S with type num_type := T.num_type
