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
    | V_Bool _ -> Ty.inferred T_Bool
    | V_Int32 _ -> Ty.inferred T_Int32
    | V_String _ -> Ty.inferred T_String
    | V_Tuple { tuple } ->
        Ty.inferred <| T_Tuple { tuple = Tuple.map ty_of tuple }
    | V_Ty _ -> Ty.inferred T_Ty
    | V_Fn { def = { arg; body; evaled_result = _ }; captured = _ } ->
        Ty.inferred <| T_Fn { arg = arg.data.ty; result = body.data.ty }
    | V_NativeFn { ty; name = _; impl = _ } -> Ty.inferred <| T_Fn ty
    | V_Error -> Ty.inferred T_Error

  let expect_ty : value -> ty =
   fun value ->
    match value.shape with
    | V_Ty ty -> ty
    | _ -> fail "expected ty, got %a" print_value value

  let expect_string : value -> string =
   fun value ->
    match value.shape with
    | V_String s -> s
    | _ -> fail "expected string, got %a" print_value value

  let expect_fn : value -> value_fn =
   fun value ->
    match value.shape with
    | V_Fn f -> f
    | _ -> fail "expected fn, got %a" print_value value

  let expect_tuple : value -> value_tuple =
   fun value ->
    match value.shape with
    | V_Tuple value -> value
    | _ -> fail "expected tuple, got %a" print_value value

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

  let print_with_spans = print_expr_with_spans
  let print_with_types = print_expr_with_types

  module Shape = struct
    type t = expr_shape

    let print = print_expr_shape
  end

  module Assignee = struct
    type t = assignee_expr

    let print_with_spans = print_assignee_expr_with_spans

    module Shape = struct
      type t = assignee_expr_shape

      let print = print_assignee_expr_shape
    end
  end

  type assignee = Assignee.t

  module Ty = struct
    type t = ty_expr

    let print = print_ty_expr

    module Shape = struct
      type t = ty_expr_shape

      let print = print_ty_expr_shape
    end
  end

  type ty = Ty.t
end

type expr = Expr.t

module Pattern = struct
  type t = pattern

  let print_with_spans = print_pattern_with_spans

  module Shape = struct
    type t = pattern_shape

    let print = print_pattern_shape
  end
end

type pattern = Pattern.t

module InterpreterScope = struct
  type t = interpreter_scope

  module Locals = struct
    type t = interpreter_locals
  end

  type locals = Locals.t
end

module Binding = struct
  type t = binding

  let print = print_binding
end

type binding = Binding.t

module Ir_data = struct
  type t = ir_data
end

type ir_data = Ir_data.t
