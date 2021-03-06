module type TYPE = sig
  type num_type
  (** num_type is the type of element of matrix *)
end

module type S = sig
  type (+'row, +'col, 'd) t

  type num_type

  val make : row:'a Size.t -> col:'b Size.t -> init:num_type -> ('a Size.t, 'b Size.t, num_type) t
  (** Make a matrix having specified rows and columns. *)

  val init : row:'a Size.t -> col:'b Size.t -> f:(int -> int -> num_type) -> ('a Size.t, 'b Size.t, num_type) t
  (** Make a matrix that is initialized via initialize function. *)

  val row_size : ('a Size.t, 'b Size.t, num_type) t -> 'a Size.t
  (** Get the size of row of given matrix. *)

  val col_size : ('a Size.t, 'b Size.t, num_type) t -> 'b Size.t
  (** Get the size of col of given matrix. *)

  val get : row:int -> col:int -> ('a Size.t, 'b Size.t, num_type) t -> num_type option
  (** Get the element of the matrix that is specified row and column. *)

  val set : row:int -> col:int -> v:num_type -> ('a Size.t, 'b Size.t, num_type) t -> unit
  (** Set the value to the matrix at specified row and column. *)

  val row_of_mat : row:int -> ('a Size.t, 'b Size.t, 'c) t -> 'c list
  (** [row_of_mat ~row mat] get the row as list of the matrix. If [row] is greater or less than number of row of the
      [mat], raise exception. *)

  val col_of_mat : col:int -> ('a Size.t, 'b Size.t, 'c) t -> 'c list
  (** [col_of_mat ~col mat] get the columns as list of the matrix. If [col] is greater or less than number of columns of
      the [mat], raise exception. *)

  val transpose : ('a Size.t, 'b Size.t, num_type) t -> ('b Size.t, 'a Size.t, num_type) t
  (** Transpose the matrix. *)

  val map : f:(int -> int -> 'c -> 'd) -> ('a Size.t, 'b Size.t, 'c) t -> ('a Size.t, 'b Size.t, 'd) t
  (** [map ~f mat] apply [f] each element of the matrix. The [f] given row and column index and value at the row and
      column in the matrix.*)

  val to_array : ('a, 'b, num_type) t -> num_type array array
  (** [to_array mat] get the two dimensional array of matrix. *)

  val to_list : ('a, 'b, num_type) t -> num_type list list
  (** [to_list mat] get the two dimensional list of matrix. *)

  val copy : ('a, 'b, num_type) t -> ('a, 'b, num_type) t
  (** [copy mat] get the new matrix to be copied from [mat] *)

  val submatrix :
    ?start_row:int ->
    ?start_col:int ->
    row:'a Size.t ->
    col:'b Size.t ->
    ('c Size.t, 'd Size.t, num_type) t ->
    ('a Size.t, 'b Size.t, num_type) t option
  (** [submatrix ~start_pos ~size m] get a submatrix is size of [size] from the [m] at [start_pos]. If [start_pos] and
      [size] gets over the size of [m], you get [None]. *)
end
