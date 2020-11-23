module Ext = Ext
module Size = Size
module Vec = Vector
module Mat = Matrix

(* Re-Packaging algebra_* packages into Algebra. *)
module Algebra : sig
  include module type of struct
    include Algebra
  end

  module Line = Algebra_line
  module Vec = Algebra_vec
  module Mat = Algebra_mat
end = struct
  include Algebra
  module Line = Algebra_line
  module Vec = Algebra_vec
  module Mat = Algebra_mat
end

module Util = Util
