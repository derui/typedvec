(** Ext_affine provides functions to operations of Affine conversion via vector and matrix.

    Provided operations are as follows;
    - translation

    @author derui
    @version 0.1
*)

val translation : ?dir:[`Row | `Col] -> ('s Algebra.s) Algebra.vec -> ('s Algebra.s, 's Algebra.s) Algebra.mat
(** [transiation vec] gets the Translation matrix is moved to [vec]. What default argument [~dir] is [`Row].
    [transiation ~dir:`Row vec] gets the Translation matrix to multiply row vector.
    [transiation ~dir:`Col vec] gets the Translation matrix to multiply column vector.

    Notice: the [vec] should be a homogeneous coordinate.
*)

