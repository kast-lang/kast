open Std
open Kast_util
open Kast_types
open Common

let construct_tuple ~span (ty : Types.ty_tuple) (tuple : (ty -> value) tuple) : value =
  with_return (fun ({ return } : Value.shape return_handle) : Value.shape ->
    let zipped =
      try Tuple.zip_order_a ty.tuple tuple with
      | Invalid_argument s ->
        Error.error span "%S" s;
        return V_Error
    in
    V_Tuple
      { ty
      ; tuple =
          zipped
          |> Tuple.map
               (fun
                   ((ty_field, value) : Types.ty_tuple_field * (ty -> value))
                    : Types.value_tuple_field
                  ->
                  let value = value ty_field.ty in
                  { place = Place.init ~mut:Inherit value; span; ty_field })
      })
  |> Value.inferred ~span
;;

let construct_variant
      ~span
      (ty : Types.ty_variant)
      (variant : string)
      (data : (ty -> value) option)
  : value
  =
  with_return (fun ({ return } : Value.shape return_handle) : Value.shape ->
    match ty.variants |> Row.await_find_opt variant with
    | None ->
      Error.error span "Did not find variant %a" String.print_debug variant;
      V_Error
    | Some (label, data_ty) ->
      let data =
        match data, data_ty.data with
        | None, None -> None
        | None, Some _ ->
          Error.error span "Variant expected data, got no data";
          return V_Error
        | Some _, None ->
          Error.error span "Variant expected no data, got some data";
          return V_Error
        | Some data, Some data_ty ->
          let data = data data_ty in
          Value.ty_of data |> Inference.Ty.expect_inferred_as ~span data_ty;
          Some data
      in
      V_Variant { label; data = data |> Option.map (Place.init ~mut:Inherit); ty })
  |> Value.inferred ~span
;;

let init () =
  [ native_fn "reflection.type_info" (fun ty_fn ~caller:span ~state:_ args ->
      let type_info_ty =
        ty_fn.result |> Ty.await_inferred |> Ty.Shape.expect_variant |> Option.unwrap
      in
      let arg = single_arg ~span args in
      match arg |> Value.expect_ty with
      | Some ty ->
        let shape = ty |> Ty.await_inferred in
        (match shape with
         | T_Unit -> construct_variant ~span type_info_ty "Unit" None
         | T_Bool -> construct_variant ~span type_info_ty "Bool" None
         | T_Int32 -> construct_variant ~span type_info_ty "Int32" None
         | T_Int64 -> construct_variant ~span type_info_ty "Int64" None
         | T_Float64 -> construct_variant ~span type_info_ty "Float64" None
         | T_String -> construct_variant ~span type_info_ty "String" None
         | T_Char -> construct_variant ~span type_info_ty "Char" None
         | T_Ref { mut; referenced } ->
           construct_variant
             ~span
             type_info_ty
             "Ref"
             (Some
                (fun data_ty ->
                  let data_ty =
                    data_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
                  in
                  construct_tuple ~span data_ty
                  <| Tuple.make
                       []
                       [ ( "mutable"
                         , fun _ ->
                             V_Bool (mut |> IsMutable.await_inferred)
                             |> Value.inferred ~span )
                       ; ("referenced", fun _ -> V_Ty referenced |> Value.inferred ~span)
                       ]))
         | T_Variant _ -> failwith __LOC__
         | T_Tuple _ -> failwith __LOC__
         | T_Ty -> failwith __LOC__
         | T_Fn _ -> failwith __LOC__
         | T_Generic _ -> failwith __LOC__
         | T_Ast -> failwith __LOC__
         | T_UnwindToken _ -> failwith __LOC__
         | T_Target -> failwith __LOC__
         | T_ContextTy -> failwith __LOC__
         | T_CompilerScope -> failwith __LOC__
         | T_Opaque _ -> failwith __LOC__
         | T_Blocked _ -> failwith __LOC__
         | T_Error -> failwith __LOC__)
      | None ->
        Error.error span "reflection.type_info needs type as arg, got %a" Value.print arg;
        V_Error |> Value.inferred ~span)
  ]
;;
