open Std
open Kast_util
open Print
open Types
module Inference = Kast_inference_base

let error = Inference.Error.error

let rec _unused () = ()

and unite_ty_shape : span:span -> ty_shape -> ty_shape -> ty_shape =
 fun ~span a b ->
  let fail () : ty_shape =
    error span "type check failed: %a != %a" print_ty_shape a print_ty_shape b;
    T_Error
  in
  match (a, b) with
  | T_Error, smth | smth, T_Error -> smth
  | T_Unit, T_Unit -> T_Unit
  | T_Unit, _ -> fail ()
  | T_Bool, T_Bool -> T_Bool
  | T_Bool, _ -> fail ()
  | T_Int32, T_Int32 -> T_Int32
  | T_Int32, _ -> fail ()
  | T_Char, T_Char -> T_Char
  | T_Char, _ -> fail ()
  | T_String, T_String -> T_String
  | T_String, _ -> fail ()
  | T_Tuple { tuple = a }, T_Tuple { tuple = b } -> (
      try
        T_Tuple
          {
            tuple =
              Tuple.zip_order_a a b
              |> Tuple.map
                   (fun
                     ((a, b) : Types.ty_tuple_field * Types.ty_tuple_field)
                     :
                     Types.ty_tuple_field
                   ->
                     {
                       ty = unite_ty ~span a.ty b.ty;
                       label = Label.unite a.label b.label;
                     });
          }
      with Invalid_argument _ -> fail ())
  | T_Tuple _, _ -> fail ()
  | T_Variant { variants = a }, T_Variant { variants = b } ->
      with_return (fun { return } : ty_shape ->
          T_Variant
            {
              variants =
                Row.unite ~span
                  (fun ~span ({ data = a } : Types.ty_variant_data)
                       ({ data = b } : Types.ty_variant_data) :
                       Types.ty_variant_data ->
                    {
                      data =
                        (match (a, b) with
                        | None, None -> None
                        | Some _, None | None, Some _ -> return <| fail ()
                        | Some a, Some b -> Some (unite_ty ~span a b));
                    })
                  a b;
            })
  | T_Variant _, _ -> fail ()
  | T_Ty, T_Ty -> T_Ty
  | T_Ty, _ -> fail ()
  | T_Ast, T_Ast -> T_Ast
  | T_Ast, _ -> fail ()
  | T_UnwindToken { result = a }, T_UnwindToken { result = b } ->
      T_UnwindToken { result = unite_ty ~span a b }
  | T_UnwindToken _, _ -> fail ()
  | T_Fn a, T_Fn b ->
      T_Fn
        {
          arg = unite_ty ~span a.arg b.arg;
          result = unite_ty ~span a.result b.result;
        }
  | T_Fn _, _ -> fail ()
  | T_Generic { def = a }, T_Generic { def = b } when a == b ->
      T_Generic { def = a }
  | T_Generic _, _ -> fail ()
  | T_Target, T_Target -> T_Target
  | T_Target, _ -> fail ()
  | T_ContextTy, T_ContextTy -> T_ContextTy
  | T_ContextTy, _ -> fail ()
  | T_CompilerScope, T_CompilerScope -> T_CompilerScope
  | T_CompilerScope, _ -> fail ()
  | T_Binding a, T_Binding b when a.id = b.id -> T_Binding a
  | T_Binding _, _ -> fail ()

and unite_ty : ty Inference.unite =
 fun ~span { var = a } { var = b } ->
  { var = Inference.Var.unite ~span unite_ty_shape a b }

and unite_value_shape : value_shape Inference.unite =
 fun ~span a b ->
  let fail () : value_shape =
    Inference.Error.error span "%a != %a" print_value_shape a print_value_shape
      b;
    V_Error
  in
  match (a, b) with
  | V_Error, smth | smth, V_Error -> smth
  | V_Unit, V_Unit -> V_Unit
  | V_Unit, _ -> fail ()
  | V_Bool a, V_Bool b when a = b -> V_Bool a
  | V_Bool _, _ -> fail ()
  | V_Int32 a, V_Int32 b when a = b -> V_Int32 a
  | V_Int32 _, _ -> fail ()
  | V_Char a, V_Char b when a = b -> V_Char a
  | V_Char _, _ -> fail ()
  | V_String a, V_String b when a = b -> V_String a
  | V_String _, _ -> fail ()
  | V_Tuple _, _ -> fail () (* TODO *)
  | V_Variant _, _ -> fail () (* TODO *)
  | V_Ty a, V_Ty b -> V_Ty (unite_ty ~span a b)
  | V_Ty _, _ -> fail ()
  | V_Fn _, _ -> fail ()
  | V_Generic _, _ -> fail ()
  | V_NativeFn _, _ -> fail () (* TODO *)
  | V_Ast _, _ -> fail ()
  | ( V_UnwindToken { result_ty = ty_a; id = id_a },
      V_UnwindToken { result_ty = ty_b; id = id_b } )
    when id_a = id_b ->
      V_UnwindToken { result_ty = unite_ty ~span ty_a ty_b; id = id_a }
  | V_UnwindToken _, _ -> fail ()
  | V_Target _, _ -> fail ()
  | V_ContextTy _, _ -> fail ()
  | V_Binding a, V_Binding b when a.id = b.id -> V_Binding a
  | V_Binding _, _ -> fail ()
  | V_CompilerScope _, _ -> fail ()

and inferred_ty ~span shape : ty =
  { var = Inference.Var.new_inferred ~span shape }

and inferred_value ~span shape : value =
  { var = Inference.Var.new_inferred ~span shape; ty = ty_of_value_shape shape }

and infer_value_shape : span:span -> ty_shape -> value_shape option =
 fun ~span ty_shape ->
  match ty_shape with
  | T_Unit -> Some V_Unit
  | T_Bool -> None
  | T_Int32 -> None
  | T_Char -> None
  | T_String -> None
  | T_Variant _ -> None
  | T_Tuple { tuple } ->
      Some
        (V_Tuple
           {
             tuple =
               tuple
               |> Tuple.map (fun (field : ty_tuple_field) : value_tuple_field ->
                   {
                     value = new_not_inferred_value_of_ty ~span field.ty;
                     span;
                     ty_field = field;
                   });
           })
  | T_Ty -> Some (V_Ty (new_not_inferred_ty ~span))
  | T_Fn _ -> None
  | T_Generic _ -> None
  | T_Ast -> None
  | T_UnwindToken _ -> None
  | T_Target -> None
  | T_ContextTy -> None
  | T_CompilerScope -> None
  | T_Binding _ -> None
  | T_Error -> None

and new_not_inferred_ty ~span : ty =
  { var = Inference.Var.new_not_inferred ~span }

and new_not_inferred_value ~span : value =
  let var = Inference.Var.new_not_inferred ~span in
  let ty = new_not_inferred_ty ~span in
  ty.var
  |> Inference.Var.once_inferred (fun ty_shape ->
      match infer_value_shape ~span ty_shape with
      | Some shape ->
          var
          |> Inference.Var.infer_as unite_value_shape shape
               ~span:(Span.fake "<infer_value_based_on_ty>")
      | None -> ());
  { var; ty }

and new_not_inferred_value_of_ty ~span ty : value =
  let value = new_not_inferred_value ~span in
  unite_ty ~span value.ty ty |> ignore;
  value

and ty_of_value_shape : value_shape -> ty =
 fun shape ->
  let span = Span.fake "<ty_of_shape>" in
  match shape with
  | V_Unit -> inferred_ty ~span T_Unit
  | V_Bool _ -> inferred_ty ~span T_Bool
  | V_Int32 _ -> inferred_ty ~span T_Int32
  | V_Char _ -> inferred_ty ~span T_Char
  | V_String _ -> inferred_ty ~span T_String
  | V_Tuple { tuple } ->
      inferred_ty ~span
      <| T_Tuple
           {
             tuple =
               Tuple.map
                 (fun (field : value_tuple_field) -> field.ty_field)
                 tuple;
           }
  | V_Variant { ty; _ } -> ty
  | V_Ty _ -> inferred_ty ~span T_Ty
  | V_Fn { ty; _ } -> inferred_ty ~span <| T_Fn ty
  | V_Generic { id; fn } -> inferred_ty ~span <| T_Generic { def = fn.def }
  | V_NativeFn { id = _; ty; name = _; impl = _ } ->
      inferred_ty ~span <| T_Fn ty
  | V_Ast _ -> inferred_ty ~span T_Ast
  | V_UnwindToken { result_ty; id = _ } ->
      inferred_ty ~span <| T_UnwindToken { result = result_ty }
  | V_Target _ -> inferred_ty ~span T_Target
  | V_ContextTy _ -> inferred_ty ~span T_ContextTy
  | V_Binding binding -> binding.ty
  | V_CompilerScope _ -> inferred_ty ~span T_CompilerScope
  | V_Error -> inferred_ty ~span T_Error
