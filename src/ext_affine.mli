(** Ext_affine provides type and operations to fold affine transformations and get the folded matrix.

    Provided operations are as follows;

    - translation
    - rotation
    - scaling

    @author derui
    @version 0.1 *)

type 'a t
(** The type of affine *)

val make : 'a Algebra.s -> 'a t
(** [make size] make a affine data type for [size] dimension. Internally, affine data type has a matrix having [size] +
    1 dimension to calculate affine transformation. *)

val translate : 'a t -> vec:'a Algebra.s Algebra.vec -> 'a t
(** [translate affine ~vec] add translation matrix to [affine]. What multiple apply this get concatted translations. *)

val rotate : 'a t -> rotate:('a Algebra.s, 'a Algebra.s) Algebra.mat -> 'a t
(** [rotate affine ~rotate] multiply rotation matrix to [affine]. What multiple apply this get rotated affine
    transformation that applied each rotate matrix. *)

val scale : 'a t -> scale:'a Algebra.s Algebra.vec -> 'a t
(** [scale ~scale affine] get affine data type multiplied a scaling matrix. *)

val to_mat : 'a t -> ('a Size.s Algebra.s, 'a Size.s Algebra.s) Algebra.mat
(** [to_mat affine] get the transformation matrix multiplied all transformation matrixs, [translate], [rotate] and
    [scale]. *)

val of_mat : size:'a Algebra.s -> ('a Size.s Algebra.s, 'a Size.s Algebra.s) Algebra.mat -> 'a t
(** [of_mat mat] make the type of Affine from a matrix.*)
