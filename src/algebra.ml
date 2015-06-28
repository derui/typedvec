type num_type' = float
type num_type = num_type'

module Mat = Matrix.Make(struct
  type num_type = num_type'
end)

module Vec = Vector.Make(struct
  type num_type = num_type'
end)

type (+'row, 'col) mat = ('row, 'col, num_type) Mat.t

type +'s vec = ('s, num_type) Vec.t

let cross ~left ~right = failwith "not implemented"
