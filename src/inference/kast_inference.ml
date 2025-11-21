open Std
open Kast_util
open Kast_types
include Kast_inference_base
include Error

let rec _unused = ()

and unite_ty_shape : span:span -> Ty.Shape.t -> Ty.Shape.t -> Ty.Shape.t =
 fun ~span a b ->
  let fail () : Ty.Shape.t =
    error span "type check failed: %a != %a" Ty.Shape.print a Ty.Shape.print b;
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
      with_return (fun { return } : Ty.Shape.t ->
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

and unite_ty : ty unite =
 fun ~span { var = a } { var = b } ->
  { var = Var.unite ~span unite_ty_shape a b }

let expect ~span unite expected x = ignore <| unite ~span expected x

module Ty = struct
  let expect_inferred_as ~span expected ty = expect ~span unite_ty expected ty
end
