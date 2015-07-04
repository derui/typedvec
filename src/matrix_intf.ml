
module type TYPE = sig
  type num_type
(* num_type is the type of element of matrix *)
end

module type S = sig

  type (+'row, 'col, 'd) t
  type num_type

  (** Make a matrix having specified rows and columns. *)
  val make :
    row:'a Size.t -> col:'b Size.t -> init:num_type -> ('a Size.t, 'b Size.t, num_type) t

  (** Make a matrix that is initialized via initializer. *)
  val init : row:'a Size.t -> col:'b Size.t -> f:(int -> int -> num_type) -> ('a Size.t, 'b Size.t, num_type) t

  (** Get the size of row of given matrix. *)
  val row_size : ('a Size.t, 'b Size.t, num_type) t -> 'a Size.t

  (** Get the size of col of given matrix. *)
  val col_size : ('a Size.t, 'b Size.t, num_type) t -> 'b Size.t

  (** Get the element of the matrix that is specified row and column. *)
  val get : row:int -> col:int -> ('a Size.t, 'b Size.t, num_type) t -> num_type option

  (** Set the value to the matrix at specified row and column. *)
  val set : row:int -> col:int -> v:num_type -> ('a Size.t, 'b Size.t, num_type) t -> unit

  (** Transpose the matrix. *)
  val transpose: ('a Size.t, 'b Size.t, num_type) t -> ('b Size.t, 'a Size.t, num_type) t

  val map: f:(int -> int -> 'c -> 'd) -> ('a Size.t, 'b Size.t, 'c) t -> ('a Size.t, 'b Size.t, 'd) t
(* [map ~f mat] apply [f] each element of the matrix. The [f] given row and column index and value at
   the row and column in the matrix.*)

end
