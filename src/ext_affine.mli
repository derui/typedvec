(** Ext_affine provides functions to operations of Affine conversion via vector and matrix.

    Provided operations are as follows;
    - translation

    @author derui
    @version 0.1
*)

val translation_to_mat : ?dir:[`Row | `Col] -> ('s Algebra.s) Algebra.vec -> ('s Algebra.s, 's Algebra.s) Algebra.mat
(** [transiation_to_mat vec] gets the Translation matrix is moved to [vec]. What default argument [~dir] is [`Row].
    [transiation_to_mat ~dir:`Row vec] gets the Translation matrix to multiply row vector.
    [transiation_to_mat ~dir:`Col vec] gets the Translation matrix to multiply column vector.

    Notice: the [vec] should be a homogeneous coordinate.
*)

val translation_of_mat: ?dir:[`Row | `Col] -> ('s Algebra.s, 's Algebra.s) Algebra.mat ->
  ('s Algebra.s) Algebra.vec
(** [translation_of_mat ?dir mat] get the vector of translation part of the matrix.
    What default argument [?dir] is [`Row].

    [translation_of_mat ~dir:`Row mat] get bottom-edge row vector.
    [translation_of_mat ~dir:`Col mat] get right-edge column vector.
*)
