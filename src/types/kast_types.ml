open Std
open Kast_util
open Types
open Print
module Types = Types
module Label = Label
module Row = Row

module Ty = struct
  type t = ty

  let print = print_ty
  let new_not_inferred () : ty = { var = Inference.Var.new_not_inferred () }
  let never = new_not_inferred

  let inferred (shape : ty_shape) : ty =
    { var = Inference.Var.new_inferred shape }

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

  let rec ty_of : value -> ty =
   fun { shape } ->
    match shape with
    | V_Unit -> Ty.inferred T_Unit
    | V_Bool _ -> Ty.inferred T_Bool
    | V_Int32 _ -> Ty.inferred T_Int32
    | V_Char _ -> Ty.inferred T_Char
    | V_String _ -> Ty.inferred T_String
    | V_Tuple { tuple } ->
        Ty.inferred
        <| T_Tuple
             {
               tuple =
                 Tuple.map
                   (fun (field : value_tuple_field) -> field.ty_field)
                   tuple;
             }
    | V_Variant { ty; _ } -> ty
    | V_Ty _ -> Ty.inferred T_Ty
    | V_Fn { ty; _ } -> Ty.inferred <| T_Fn ty
    | V_Generic { fn } -> Ty.inferred <| T_Generic { def = fn.def }
    | V_NativeFn { ty; name = _; impl = _ } -> Ty.inferred <| T_Fn ty
    | V_Ast _ -> Ty.inferred T_Ast
    | V_UnwindToken { result_ty; id = _ } ->
        Ty.inferred (T_UnwindToken { result = result_ty })
    | V_Target _ -> Ty.inferred T_Target
    | V_ContextTy _ -> Ty.inferred T_ContextTy
    | V_Binding binding -> binding.ty
    | V_CompilerScope _ -> Ty.inferred T_CompilerScope
    | V_Error -> Ty.inferred T_Error

  let expect_unit : value -> unit option =
   fun value ->
    match value.shape with
    | V_Unit -> Some ()
    | _ -> None

  let expect_unwind_token : value -> Types.value_unwind_token option =
   fun value ->
    match value.shape with
    | V_UnwindToken token -> Some token
    | _ -> None

  let expect_bool : value -> bool option =
   fun value ->
    match value.shape with
    | V_Bool value -> Some value
    | _ -> None

  let expect_int32 : value -> int32 option =
   fun value ->
    match value.shape with
    | V_Int32 value -> Some value
    | _ -> None

  let expect_ty : value -> ty option =
   fun value ->
    match value.shape with
    | V_Binding binding -> Some (Ty.inferred (T_Binding binding))
    | V_Ty ty -> Some ty
    | _ -> None

  let expect_string : value -> string option =
   fun value ->
    match value.shape with
    | V_String s -> Some s
    | _ -> None

  let expect_fn : value -> value_fn option =
   fun value ->
    match value.shape with
    | V_Fn f -> Some f
    | _ -> None

  let expect_tuple : value -> value_tuple option =
   fun value ->
    match value.shape with
    | V_Tuple value -> Some value
    | _ -> None

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
