(* A data type of vector *)
type (+'n, 'd) t

(* Make specified dimension vector with initial value *)
val make : 'a Size.t -> 'b -> ('a Size.t, 'b) t

(* Make specified dimension vector with values created via initial funciton *)
val init : 'a Size.t -> (int -> 'b) -> ('a Size.t, 'b) t

(* Get size of the vector *)
val size : ('a, 'b) t -> 'a

(* Get element of the vector at specified index that is 0 origin. *)
val get : ('a, 'b) t -> int -> 'b

(* Set element to the vector at specified index that is 0 origin. *)
val set : ('a, 'b) t -> int -> 'b -> unit

(* # Iterations *)
val map : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val iter : f:('b -> unit) -> ('a, 'b) t -> unit
val mapi: f:(int -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val iteri : f:(int -> 'b -> unit) -> ('a, 'b) t -> unit

val fold_left: f:('a -> 'b -> 'a) -> init:'a -> ('n, 'b) t -> 'a
val fold_right: f:('a -> 'b -> 'b) -> init:'b -> ('n, 'a) t -> 'b

(* Get new array converted from vector inner data structure. *)
val to_list : ('a, 'b) t -> 'b list


(* Predications. These functions has similarity behaviour in List module. *)
val for_all : f:('b -> bool) -> ('a, 'b) t -> bool
val exists: f:('b -> bool) -> ('a, 'b) t -> bool
val mem: member:'b -> ('a, 'b) t -> bool
val memq: member:'b -> ('a, 'b) t -> bool
