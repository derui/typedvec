module type TYPE = sig
  type num_type
  (** num_type is the type of element of vector. *)

  val compare : num_type -> num_type -> int
  (** [compare v1 v2] get the greater or less, or equals between [v1] and [v2]. *)
end

module type S = sig
  type (+'n, 'd) t
  (** A data type of vector *)

  type num_type

  val make : 'a Size.t -> num_type -> ('a Size.t, num_type) t
  (** Make specified dimension vector with initial value *)

  val init : 'a Size.t -> (int -> num_type) -> ('a Size.t, num_type) t
  (** Make specified dimension vector with values created via initial funciton *)

  val size : ('a, num_type) t -> 'a
  (** Get size of the vector *)

  val get : ('a, num_type) t -> index:int -> num_type option
  (** Get element of the vector at specified index that is 0 origin. *)

  val set : ('a, num_type) t -> index:int -> v:num_type -> unit
  (** Set element to the vector at specified index that is 0 origin. *)

  val unsafe_get : ('a, num_type) t -> int -> num_type
  (** Get element of the vector at specified index, this do not check boundary of the array *)

  val unsafe_set : ('a, num_type) t -> int -> num_type -> unit
  (** Set element of the vector at specified index, this do not check boundary of the array *)

  val map : f:(num_type -> 'c) -> ('a, num_type) t -> ('a, 'c) t
  (** # Iterations *)

  val iter : f:(num_type -> unit) -> ('a, num_type) t -> unit

  val mapi : f:(int -> num_type -> 'c) -> ('a, num_type) t -> ('a, 'c) t

  val iteri : f:(int -> num_type -> unit) -> ('a, num_type) t -> unit

  val map2 : f:(num_type -> num_type -> 'c) -> v1:('a, num_type) t -> v2:('a, num_type) t -> ('a, 'c) t
  (** Iterations with 2 vector2 *)

  val iter2 : f:(num_type -> num_type -> unit) -> v1:('a, num_type) t -> v2:('a, num_type) t -> unit

  val zip : v1:('a, num_type) t -> v2:('a, 'c) t -> ('a, num_type * 'c) t

  val fold_left : f:('a -> num_type -> 'a) -> init:'a -> ('n, num_type) t -> 'a

  val fold_right : f:(num_type -> 'b -> 'b) -> init:'b -> ('n, num_type) t -> 'b

  val to_list : ('a, 'b) t -> 'b list
  (** Get new array converted from vector inner data structure. *)

  val for_all : f:(num_type -> bool) -> ('a, num_type) t -> bool
  (** Predications. These functions has similarity behaviour in List module. *)

  val exists : f:(num_type -> bool) -> ('a, num_type) t -> bool

  val mem : member:num_type -> ('a, num_type) t -> bool

  val memq : member:num_type -> ('a, num_type) t -> bool

  (** # Copy vector *)

  val copy : ?y:('a, num_type) t -> ('a, num_type) t -> ('a, num_type) t
  (** [copy ?y v] copy [v] to [y] or new vector which new allocate in copy. *)

  val equals : ('a, num_type) t -> ('a, num_type) t -> bool
  (** [equals v1 v2] get the result if equals between [v1] and [v2]. *)
end
