open Types
open Print

module Value = struct
  type t = value

  let print = print_value

  module Shape = struct
    type t = value_shape

    let print = print_value_shape
  end

  type shape = Shape.t
end

type value = Value.t

module Ty = struct
  type t = ty

  let print = print_ty

  module Shape = struct
    type t = ty_shape

    let print = print_ty_shape
  end
end

type ty = Ty.t

module Expr = struct
  type t = expr

  module Shape = struct
    type t = expr_shape
  end

  module Assignee = struct
    type t = assignee_expr

    module Shape = struct
      type t = assignee_expr_shape
    end
  end

  type assignee = Assignee.t
end

type expr = Expr.t

module Pattern = struct
  type t = pattern
end

type pattern = Pattern.t
