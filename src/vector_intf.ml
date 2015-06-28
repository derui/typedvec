module type TYPE = sig
  type num_type
(* num_type is the type of element of vector. *)
end

module type S = sig
  (* A data type of vector *)
  type (+'n, 'd) t
  type num_type

  (* Make specified dimension vector with initial value *)
  val make : 'a Size.t -> num_type -> ('a Size.t, num_type) t

  (* Make specified dimension vector with values created via initial funciton *)
  val init : 'a Size.t -> (int -> num_type) -> ('a Size.t, num_type) t

  (* Get size of the vector *)
  val size : ('a, num_type) t -> 'a

  (* Get element of the vector at specified index that is 0 origin. *)
  val get : ('a, num_type) t -> int -> num_type option

  (* Set element to the vector at specified index that is 0 origin. *)
  val set : ('a, num_type) t -> int -> num_type -> unit

  (* Get element of the vector at specified index, this do not check boundary of the array *)
  val unsafe_get : ('a, num_type) t -> int -> num_type

  (* Set element of the vector at specified index, this do not check boundary of the array *)
  val unsafe_set : ('a, num_type) t -> int -> num_type -> unit

  (* # Iterations *)
  val map : f:(num_type -> 'c) -> ('a, num_type) t -> ('a, 'c) t
  val iter : f:(num_type -> unit) -> ('a, num_type) t -> unit
  val mapi: f:(int -> num_type -> 'c) -> ('a, num_type) t -> ('a, 'c) t
  val iteri : f:(int -> num_type -> unit) -> ('a, num_type) t -> unit

  (* Iterations with 2 vector2 *)
  val map2: f:(num_type -> num_type -> 'c) -> v1:('a, num_type) t -> v2:('a, num_type) t -> ('a, 'c) t
  val iter2: f:(num_type -> num_type -> unit) -> v1:('a, num_type) t -> v2:('a, num_type) t -> unit

  val zip: v1:('a, num_type) t -> v2:('a, 'c) t -> ('a, num_type * 'c) t

  val fold_left: f:('a -> num_type -> 'a) -> init:'a -> ('n, num_type) t -> 'a
  val fold_right: f:(num_type -> 'b -> 'b) -> init:'b -> ('n, num_type) t -> 'b

  (* Get new array converted from vector inner data structure. *)
  val to_list : ('a, 'b) t -> 'b list

  (* Predications. These functions has similarity behaviour in List module. *)
  val for_all : f:(num_type -> bool) -> ('a, num_type) t -> bool
  val exists: f:(num_type -> bool) -> ('a, num_type) t -> bool
  val mem: member:num_type -> ('a, num_type) t -> bool
  val memq: member:num_type -> ('a, num_type) t -> bool

  (* # Copy vector *)

  (* [copy ?y v] copy [v] to [y] or new vector which new allocate in copy. *)
  val copy: ?y:('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
end
