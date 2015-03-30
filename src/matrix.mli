type (+'row, 'col, 'd) t
val make :
  row:'a Size.t -> col:'b Size.t -> init:'c -> ('a Size.t, 'b Size.t, 'c) t
val row_size : ('a Size.t, 'b Size.t, 'c) t -> 'a Size.t
val col_size : ('a Size.t, 'b Size.t, 'c) t -> 'b Size.t
val get_row : row:int -> ('a Size.t, 'b Size.t, 'c) t -> ('b Size.t, 'c) Vector.t option
val get : row:int -> col:int -> ('a Size.t, 'b Size.t, 'c) t -> 'c option

val transpose: ('a Size.t, 'b Size.t, 'c) t -> ('b Size.t, 'a Size.t, 'c) t
