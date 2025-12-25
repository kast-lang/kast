open Std
open Kast_util
open Types
open Print
module Types = Types
module Label = Label
module Row = Row
module Inference_impl = Inference_impl
module Print = Print

type 'a compiled_kind = 'a Types.compiled_kind
type compiled = Types.compiled

module Name = struct
  type t = name

  let new_inferred ~span value : name =
    { var = Inference.Var.new_inferred ~span value }

  let new_not_inferred ~span : name =
    { var = Inference.Var.new_not_inferred ~span }
end

module OptionalName = struct
  type t = optional_name

  let new_inferred ~span value : optional_name =
    { var = Inference.Var.new_inferred ~span value }

  let new_not_inferred ~span : optional_name =
    { var = Inference.Var.new_not_inferred ~span }
end

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
      | T_Tuple t -> Some t
      | _ -> None

    let expect_variant : t -> ty_variant option = function
      | T_Variant t -> Some t
      | _ -> None

    let name : t -> optional_name =
      let span = Span.fake "<Ty.Shape.name>" in
      function
      | T_Unit | T_Bool | T_Int32 | T_Int64 | T_Float64 | T_String | T_Char
      | T_Ref _ | T_Ty | T_Fn _ | T_Generic _ | T_Ast | T_UnwindToken _
      | T_Target | T_ContextTy | T_CompilerScope | T_Opaque _ | T_Blocked _
      | T_Error ->
          None |> OptionalName.new_inferred ~span
      | T_Variant { name; _ } | T_Tuple { name; _ } -> name
  end

  let name ty = await_inferred ty |> Shape.name
end

type ty = Ty.t

module Value = struct
  type t = value

  module Shape = struct
    type t = value_shape

    let print = print_value_shape
    let ty_of = Inference_impl.ty_of_value_shape

    let name : value_shape -> optional_name =
      let span = Span.fake "<Value.Shape.name>" in
      function
      | V_Unit | V_Bool _ | V_Int32 _ | V_Int64 _ | V_Float64 _ | V_Char _
      | V_Ref _ | V_String _ | V_Tuple _ | V_Variant _ | V_Ast _
      | V_UnwindToken _ | V_Target _ | V_ContextTy _ | V_CompilerScope _
      | V_Opaque _ | V_Blocked _ | V_Error ->
          None |> OptionalName.new_inferred ~span
      | V_Fn _ | V_NativeFn _ ->
          (* TODO *)
          None |> OptionalName.new_inferred ~span
      | V_Generic generic ->
          Some generic.name |> OptionalName.new_inferred ~span
      | V_Ty ty -> ty |> Ty.name
  end

  let new_not_inferred = Inference_impl.new_not_inferred_value
  let new_not_inferred_of_ty = Inference_impl.new_not_inferred_value_of_ty
  let inferred = Inference_impl.inferred_value
  let ty_of : value -> ty = fun { var = _; ty } -> ty

  let binding ~span binding =
    V_Blocked { shape = BV_Binding binding; ty = binding.ty } |> inferred ~span

  let await_inferred : value -> value_shape =
   fun value -> value.var |> Inference.Var.await_inferred ~error_shape:V_Error

  let name value = await_inferred value |> Shape.name

  let expect_unit : value -> unit option =
   fun value ->
    match value |> await_inferred with
    | V_Unit -> Some ()
    | _ -> None

  let expect_ref : value -> value_ref option =
   fun value ->
    match value |> await_inferred with
    | V_Ref ref -> Some ref
    | _ -> None

  let expect_opaque : 'a. value -> 'a option =
   fun value ->
    match value |> await_inferred with
    | V_Opaque { ty = _; value } -> Some (Obj.obj value)
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

  let expect_char : value -> char option =
   fun value ->
    match value |> await_inferred with
    | V_Char value -> Some value
    | _ -> None

  let expect_int32 : value -> int32 option =
   fun value ->
    match value |> await_inferred with
    | V_Int32 value -> Some value
    | _ -> None

  let expect_int64 : value -> int64 option =
   fun value ->
    match value |> await_inferred with
    | V_Int64 value -> Some value
    | _ -> None

  let expect_float64 : value -> float option =
   fun value ->
    match value |> await_inferred with
    | V_Float64 value -> Some value
    | _ -> None

  let expect_ty : value -> ty option =
    let span = Span.fake "<expect_ty>" in
    fun value ->
      let inferred = value |> await_inferred in
      match inferred with
      | V_Blocked blocked -> Some (Ty.inferred ~span (T_Blocked blocked))
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

  module Place = struct
    type t = place_expr

    let print = print_place_expr

    module Shape = struct
      type t = place_expr_shape

      let print = print_place_expr_shape
    end
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

  let print = print_pattern

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

module Place = struct
  type t = place

  let init = Inference_impl.init_place
  let init_state ~mut state ty : place = { id = Id.gen (); state; ty; mut }

  let is_mutable ~(parent_mut : bool) (place : place) : bool =
    match place.mut with
    | Immutable -> false
    | Mutable -> parent_mut
    | Inherit -> parent_mut

  let assign (value : value) (place : place) ~(parent_mut : bool) =
    if (not (is_mutable ~parent_mut place)) && place.state <> Uninitialized then
      fail "Mutated non-mut place";
    place.state <- Occupied value

  let print_value = print_place_value
  let print_ref = print_place_ref

  module Mut = struct
    type t = place_mut

    let bool : bool -> t = function
      | true -> Mutable
      | false -> Immutable

    let to_bool : t -> bool = function
      | Mutable -> true
      | Immutable -> false
      | Inherit -> fail "Place.Mut.to_bool with Inherit"
  end
end

type place = Place.t
type optional_name = OptionalName.t
