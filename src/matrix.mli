type (+'row, 'col, 'd) t
(** Make a matrix having specified rows and columns. *)
val make :
  row:'a Size.t -> col:'b Size.t -> init:'c -> ('a Size.t, 'b Size.t, 'c) t

(** Make a matrix that is initialized via initializer. *)
val init : row:'a Size.t -> col:'b Size.t -> f:(int -> int -> 'c) -> ('a Size.t, 'b Size.t, 'c) t

(** Get the size of row of given matrix. *)
val row_size : ('a Size.t, 'b Size.t, 'c) t -> 'a Size.t

(** Get the size of col of given matrix. *)
val col_size : ('a Size.t, 'b Size.t, 'c) t -> 'b Size.t

(** Get the row from the matrix. *)
val get_row : row:int -> ('a Size.t, 'b Size.t, 'c) t -> ('b Size.t, 'c) Vector.t option

(** Get the element of the matrix that is specified row and column. *)
val get : row:int -> col:int -> ('a Size.t, 'b Size.t, 'c) t -> 'c option

(** Set the value to the matrix at specified row and column. *)
val set : row:int -> col:int -> v:'c -> ('a Size.t, 'b Size.t, 'c) t -> unit

(** Transpose the matrix. *)
val transpose: ('a Size.t, 'b Size.t, 'c) t -> ('b Size.t, 'a Size.t, 'c) t
