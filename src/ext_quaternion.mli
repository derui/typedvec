(** This module provide type of Quaternion and quaternion related operations.

    Provided operations are as follows;

    - identity
    - normalize
    - multiply
    - convert to matrix Quaternion type provide from this module is depending on {!Vector} and {!Matrix4}.

    @author derui
    @version 0.1 *)

type t
(** type of quaternion. user is only using by this module functions to be intermadiate. if need information of
    quaternion, using `axis' and `angle' function of this module. *)

type axis = Size.three Algebra.s Algebra.vec
(** This type of an axis of the Quaternion. Also known imaginaly number. *)

type angle = float
(** This type of an angle of the Quaternion. *)

val identity : t
(** construct identity quaternion.*)

val make : angle:angle -> axis:axis -> unit -> t
(** construct a quaternion from angle of degree and rotation axis. if given axis is not unit vector, this function is
    using without change. To need unit axis, call `normalize' with target quaternion. *)

val normalize : t -> t
(** return a normalized quaternion. normalized quaternion has a unit axis. *)

val multiply : t -> t -> t
(** multiply first quaternion by second one. this function is precise math definition, so if some quaternion need to
    combine, multipling order is `right to left'. *)

(** This module is a helper to multiply quaternions. applying order of each function, ( *> ) is `first to second', but (
    *< ) is `second to first', if equlivalence order of quaternions give. *)
module Open : sig
  val ( *> ) : t -> t -> t

  val ( *< ) : t -> t -> t
end

val axis : t -> axis
(** extract informations from quaternion type variable. *)

val angle : t -> angle

val to_mat : t -> (Size.three Algebra.s, Size.three Algebra.s) Algebra.mat
(** Get the rotation matrix via four by four square matrix.*)

val slerp : from_quat:t -> to_quat:t -> freq:float -> unit -> t
(** construct `obversing quaternion` between from and by obversing frequency. freq is only applied with between 0.0 and
    1.0. if out of this range, raise exception. *)

val to_string : t -> string
(** Get stringified quaternion, example, (q | x y z) *)
