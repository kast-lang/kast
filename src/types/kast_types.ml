open Std
open Kast_util
open Types
open Print

module Ty = struct
  type t = ty

  let print = print_ty
  let new_not_inferred () : ty = { var = Inference.Var.new_not_inferred () }

  let inferred (shape : ty_shape) : ty =
    { var = Inference.Var.new_inferred shape }

  module Shape = struct
    type t = ty_shape

    let print = print_ty_shape
  end
end

type ty = Ty.t

module Value = struct
  type t = value

  let rec ty_of : value -> ty =
   fun { shape } ->
    match shape with
    | V_Unit -> Ty.inferred T_Unit
    | V_Int32 _ -> Ty.inferred T_Int32
    | V_String _ -> Ty.inferred T_String
    | V_Tuple { tuple } ->
        Ty.inferred @@ T_Tuple { tuple = Tuple.map ty_of tuple }
    | V_Ty _ -> Ty.inferred T_Ty
    | V_Fn { arg; body } ->
        Ty.inferred @@ T_Fn { arg = arg.ty; result = body.ty }
    | V_NativeFn { ty; name = _; impl = _ } -> Ty.inferred @@ T_Fn ty

  let print = print_value

  module Shape = struct
    type t = value_shape

    let print = print_value_shape
  end

  type shape = Shape.t
end

type value = Value.t

module Expr = struct
  type t = expr

  let print = print_expr

  module Shape = struct
    type t = expr_shape

    let print = print_expr_shape
  end

  module Assignee = struct
    type t = assignee_expr

    let print = print_assignee_expr

    module Shape = struct
      type t = assignee_expr_shape

      let print = print_assignee_expr_shape
    end
  end

  type assignee = Assignee.t
end

type expr = Expr.t

module Pattern = struct
  type t = pattern

  let print = print_pattern

  module Shape = struct
    type t = pattern_shape

    let print = print_pattern_shape
  end
end

type pattern = Pattern.t

module Binding = struct
  type t = binding

  let print = print_binding
end
