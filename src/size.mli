type 'a t = private int
(** the types of size *)

type 'a s
(** successor of size *)

type 'a p
(** predicator of size *)

type z
(** The type of zero. *)

type zero = z
(** Defined sizes as type. *)

val zero : z t

type one = z s

val one : one t

type two = z s s

val two : two t

type three = z s s s

val three : three t

type four = z s s s s

val four : four t

type five = z s s s s s

val five : five t

type six = z s s s s s s

val six : six t

type seven = z s s s s s s s

val seven : seven t

type eight = z s s s s s s s s

val eight : eight t

type nine = z s s s s s s s s s

val nine : nine t

type ten = z s s s s s s s s s s

val ten : ten t

val succ : 'a t -> 'a s t

val pred : 'a t -> 'a p t

val to_int : 'a t -> int
(** [to_int n] convert size variant to integer. *)

module type SIZE = sig
  type size

  val size : size t

  val to_int : int
end

val of_int : int -> (module SIZE)
(** [of_int n] convert integer to SIZE module which having original size type. *)

val iszero : 'a t -> bool
(** Checking *)
