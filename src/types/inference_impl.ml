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
    error span "ty_shape %a != %a" print_ty_shape a print_ty_shape b;
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
  | T_Int64, T_Int64 -> T_Int64
  | T_Int64, _ -> fail ()
  | T_Float64, T_Float64 -> T_Float64
  | T_Float64, _ -> fail ()
  | T_Char, T_Char -> T_Char
  | T_Char, _ -> fail ()
  | T_String, T_String -> T_String
  | T_String, _ -> fail ()
  | T_Ref a, T_Ref b -> T_Ref (unite_ty ~span a b)
  | T_Ref _, _ -> fail ()
  | T_Tuple a, T_Tuple b -> T_Tuple (unite_ty_tuple ~span a b)
  | T_Tuple _, _ -> fail ()
  | ( T_Variant { name = name_a; variants = a },
      T_Variant { name = name_b; variants = b } ) ->
      T_Variant
        {
          name = unite_optional_name ~span name_a name_b;
          variants = Row.unite ~span unite_ty_variant_data a b;
        }
  | T_Variant _, _ -> fail ()
  | T_Ty, T_Ty -> T_Ty
  | T_Ty, _ -> fail ()
  | T_Ast, T_Ast -> T_Ast
  | T_Ast, _ -> fail ()
  | T_UnwindToken { result = a }, T_UnwindToken { result = b } ->
      T_UnwindToken { result = unite_ty ~span a b }
  | T_UnwindToken _, _ -> fail ()
  | T_Fn a, T_Fn b -> T_Fn (unite_ty_fn ~span a b)
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

and unite_ty_tuple : ty_tuple Inference.unite =
 fun ~span ({ name = name_a; tuple = a } as tuple_a)
     ({ name = name_b; tuple = b } as tuple_b) ->
  try
    {
      name = unite_optional_name ~span name_a name_b;
      tuple =
        Tuple.zip_order_a a b
        |> Tuple.map (fun (a, b) -> unite_ty_tuple_field ~span a b);
    }
  with Invalid_argument _ ->
    Inference.Error.error span "ty_tuple %a != %a" print_ty_tuple tuple_a
      print_ty_tuple tuple_b;
    tuple_a

and unite_ty_tuple_field =
 fun ~span (a : Types.ty_tuple_field) (b : Types.ty_tuple_field) :
     Types.ty_tuple_field ->
  {
    ty = unite_ty ~span a.ty b.ty;
    label = unite_option (fun ~span:_ -> Label.unite) ~span a.label b.label;
  }

and unite_ty_variant_data =
 fun ~span ({ data = a } : Types.ty_variant_data)
     ({ data = b } : Types.ty_variant_data) : Types.ty_variant_data ->
  {
    data =
      (match (a, b) with
      | None, None -> None
      | Some ty, None | None, Some ty ->
          Inference.Error.error span "data & not data mismatch in variant";
          Some ty
      | Some a, Some b -> Some (unite_ty ~span a b));
  }

and unite_ty_fn : ty_fn Inference.unite =
 fun ~span a b ->
  {
    arg = unite_ty ~span a.arg b.arg;
    result = unite_ty ~span a.result b.result;
  }

and unite_ty : ty Inference.unite =
 fun ~span { var = a } { var = b } ->
  { var = Inference.Var.unite ~span unite_ty_shape a b }

and unite_place : place Inference.unite =
 fun ~span a b ->
  let get_value (place : place) : value =
    match place.state with
    | Uninitialized ->
        Inference.Error.error span "uninitialized place";
        V_Error |> inferred_value ~span
    | Occupied value -> value
    | MovedOut ->
        Inference.Error.error span "moved out place";
        V_Error |> inferred_value ~span
  in
  let a = get_value a in
  let b = get_value b in
  init_place (unite_value ~span a b)

and unite_value_shape : value_shape Inference.unite =
 fun ~span a b ->
  let fail () : value_shape =
    Inference.Error.error span "value_shape %a != %a" print_value_shape a
      print_value_shape b;
    V_Error
  in
  match (a, b) with
  | V_Error, smth | smth, V_Error -> smth
  | V_Ty ty, V_Binding b | V_Binding b, V_Ty ty ->
      V_Ty (unite_ty ~span ty (T_Binding b |> inferred_ty ~span))
  | V_Unit, V_Unit -> V_Unit
  | V_Unit, _ -> fail ()
  | V_Bool a, V_Bool b when a = b -> V_Bool a
  | V_Bool _, _ -> fail ()
  | V_Int32 a, V_Int32 b when a = b -> V_Int32 a
  | V_Int32 _, _ -> fail ()
  | V_Int64 a, V_Int64 b when a = b -> V_Int64 a
  | V_Int64 _, _ -> fail ()
  | V_Float64 a, V_Float64 b when a = b -> V_Float64 a
  | V_Float64 _, _ -> fail ()
  | V_Char a, V_Char b when a = b -> V_Char a
  | V_Char _, _ -> fail ()
  | V_String a, V_String b when a = b -> V_String a
  | V_String _, _ -> fail ()
  | V_Ref a, V_Ref b when Repr.equal a b -> V_Ref a
  | V_Ref _, _ -> fail ()
  | V_Tuple { ty = ty_a; tuple = a }, V_Tuple { ty = ty_b; tuple = b } ->
      V_Tuple
        {
          ty = unite_ty_tuple ~span ty_a ty_b;
          tuple =
            Tuple.zip_order_a a b
            |> Tuple.map
                 (fun
                   ((a, b) : Types.value_tuple_field * Types.value_tuple_field)
                   :
                   Types.value_tuple_field
                 ->
                   {
                     place = unite_place ~span a.place b.place;
                     span = a.span;
                     ty_field = unite_ty_tuple_field ~span a.ty_field b.ty_field;
                   });
        }
  | V_Tuple _, _ -> fail ()
  | V_Variant _, _ -> fail () (* TODO *)
  | V_Ty a, V_Ty b -> V_Ty (unite_ty ~span a b)
  | V_Ty _, _ -> fail ()
  | V_Fn a, V_Fn b when a.fn.id = b.fn.id ->
      V_Fn { ty = unite_ty_fn ~span a.ty b.ty; fn = a.fn }
  | V_Fn _, _ -> fail ()
  | V_Generic a, V_Generic b when a.id = b.id -> V_Generic a
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

and unite_value : value Inference.unite =
 fun ~span { var = a; ty = ty_a } { var = b; ty = ty_b } ->
  {
    var = Inference.Var.unite ~span unite_value_shape a b;
    ty = unite_ty ~span ty_a ty_b;
  }

and unite_optional_name : optional_name Inference.unite =
 fun ~span { var = a } { var = b } ->
  { var = Inference.Var.unite ~span (unite_option unite_name_shape) a b }

and unite_option : 'a. 'a Inference.unite -> 'a option Inference.unite =
 fun unite_value ~span a b ->
  match (a, b) with
  | Some a, Some b -> Some (unite_value ~span a b)
  | Some value, None | None, Some value ->
      Inference.Error.error span "Can't unite Some and None";
      Some value
  | None, None -> None

and unite_name_shape : name_shape Inference.unite =
 fun ~span ({ parts = a } as name_a) ({ parts = b } as name_b) ->
  if List.length a <> List.length b then (
    Inference.Error.error span "name_shape %a != %a" print_name_shape name_a
      print_name_shape name_b;
    { parts = a })
  else
    {
      parts = List.zip a b |> List.map (fun (a, b) -> unite_name_part ~span a b);
    }

and unite_name_part : name_part Inference.unite =
 fun ~span a b ->
  let fail () =
    Inference.Error.error span "name_part %a != %a" print_name_part a
      print_name_part b;
    a
  in
  match (a, b) with
  | Uri a, Uri b when Uri.equal a b -> Uri a
  | Uri _, _ -> fail ()
  | Str a, Str b when a = b -> Str a
  | Str _, _ -> fail ()
  | Symbol a, Symbol b when Symbol.equal a b -> Symbol a
  | Symbol _, _ -> fail ()
  | Instantiation a, Instantiation b -> Instantiation (unite_value ~span a b)
  | Instantiation _, _ -> fail ()

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
  | T_Int64 -> None
  | T_Float64 -> None
  | T_Char -> None
  | T_String -> None
  | T_Variant _ -> None
  | T_Ref _ -> None
  | T_Tuple ({ name = _; tuple } as ty) ->
      Some
        (V_Tuple
           {
             ty;
             tuple =
               tuple
               |> Tuple.map (fun (field : ty_tuple_field) : value_tuple_field ->
                   {
                     place =
                       init_place <| new_not_inferred_value_of_ty ~span field.ty;
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
          var |> Inference.Var.infer_as unite_value_shape shape ~span
      | None -> ());
  { var; ty }

and new_not_inferred_value_of_ty ~span ty : value =
  let value = new_not_inferred_value ~span in
  unite_ty ~span value.ty ty |> ignore;
  value

and ty_of_value_shape : value_shape -> ty =
  let span = Span.fake "<ty_of_shape>" in
  fun shape ->
    match shape with
    | V_Unit -> inferred_ty ~span T_Unit
    | V_Bool _ -> inferred_ty ~span T_Bool
    | V_Int32 _ -> inferred_ty ~span T_Int32
    | V_Int64 _ -> inferred_ty ~span T_Int64
    | V_Float64 _ -> inferred_ty ~span T_Float64
    | V_Char _ -> inferred_ty ~span T_Char
    | V_String _ -> inferred_ty ~span T_String
    | V_Ref place -> inferred_ty ~span (T_Ref place.ty)
    | V_Tuple { ty; tuple = _ } -> inferred_ty ~span <| T_Tuple ty
    | V_Variant { ty; _ } -> inferred_ty ~span <| T_Variant ty
    | V_Ty _ -> inferred_ty ~span T_Ty
    | V_Fn { ty; _ } -> inferred_ty ~span <| T_Fn ty
    | V_Generic { id = _; name = _; fn } ->
        inferred_ty ~span <| T_Generic { def = fn.def }
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

and init_place value : place =
  { id = Id.gen (); state = Occupied value; ty = value.ty }
