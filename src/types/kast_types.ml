open Std
open Kast_util
open Types
open Print
module Types = Types
module Label = Label
module Row = Row
module Inference_impl = Inference_impl

module Ty = struct
  type t = ty

  let print = print_ty
  let new_not_inferred = Inference_impl.new_not_inferred_ty
  let never = new_not_inferred
  let inferred = Inference_impl.inferred_ty

  let await_inferred : ty -> ty_shape =
   fun ty -> ty.var |> Inference.Var.await_inferred ~error_shape:T_Error

  module Shape = struct
    type t = ty_shape

    let print = print_ty_shape

    let expect_tuple : t -> ty_tuple option = function
      | T_Tuple tuple -> Some tuple
      | _ -> None
  end
end

type ty = Ty.t

module Value = struct
  type t = value

  module Shape = struct
    type t = value_shape

    let print = print_value_shape
    let ty_of = Inference_impl.ty_of_value_shape
  end

  let new_not_inferred = Inference_impl.new_not_inferred_value
  let inferred = Inference_impl.inferred_value
  let ty_of : value -> ty = fun { var = _; ty } -> ty

  let await_inferred : value -> value_shape =
   fun value -> value.var |> Inference.Var.await_inferred ~error_shape:V_Error

  let expect_unit : value -> unit option =
   fun value ->
    match value |> await_inferred with
    | V_Unit -> Some ()
    | _ -> None

  let expect_unwind_token : value -> Types.value_unwind_token option =
   fun value ->
    match value |> await_inferred with
    | V_UnwindToken token -> Some token
    | _ -> None

  let expect_bool : value -> bool option =
   fun value ->
    match value |> await_inferred with
    | V_Bool value -> Some value
    | _ -> None

  let expect_int32 : value -> int32 option =
   fun value ->
    match value |> await_inferred with
    | V_Int32 value -> Some value
    | _ -> None

  let expect_ty : value -> ty option =
   fun value ->
    let inferred = value |> await_inferred in
    match inferred with
    | V_Binding binding ->
        Some (Ty.inferred ~span:binding.span (T_Binding binding))
    | V_Ty ty -> Some ty
    | _ -> None

  let expect_string : value -> string option =
   fun value ->
    match value |> await_inferred with
    | V_String s -> Some s
    | _ -> None

  let expect_fn : value -> value_fn option =
   fun value ->
    match value |> await_inferred with
    | V_Fn f -> Some f
    | _ -> None

  let expect_tuple : value -> value_tuple option =
   fun value ->
    match value |> await_inferred with
    | V_Tuple value -> Some value
    | _ -> None

  let print = print_value

  type shape = Shape.t
end

type value = Value.t

module Expr = struct
  type t = expr

  let print_with_spans = print_expr_with_spans
  let print_with_types = print_expr_with_types
  let print_short = print_expr_short

  module Shape = struct
    type t = expr_shape
    type quote_ast_child = expr_quote_ast_child
    type quote_ast_group = expr_quote_ast_group
    type quote_ast = expr_quote_ast

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
