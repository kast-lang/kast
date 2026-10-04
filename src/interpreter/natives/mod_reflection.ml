open Common

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
         | T_UInt32 -> construct_variant ~span type_info_ty "UInt32" None
         | T_Int64 -> construct_variant ~span type_info_ty "Int64" None
         | T_UInt64 -> construct_variant ~span type_info_ty "UInt64" None
         | T_Float32 -> construct_variant ~span type_info_ty "Float32" None
         | T_Float64 -> construct_variant ~span type_info_ty "Float64" None
         | T_String -> construct_variant ~span type_info_ty "String" None
         | T_StringView -> construct_variant ~span type_info_ty "StringView" None
         | T_Char -> construct_variant ~span type_info_ty "Char" None
         | T_Box boxed ->
           construct_variant
             ~span
             type_info_ty
             "Box"
             (Some (fun _ -> V_Ty boxed |> Value.inferred ~span))
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
         | T_Variant ty_variant ->
           construct_variant
             ~span
             type_info_ty
             "Variant"
             (Some
                (fun data_ty ->
                  let data_ty =
                    data_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
                  in
                  construct_tuple ~span data_ty
                  <| Tuple.make
                       []
                       [ ( "variants"
                         , fun list_ty ->
                             construct_slist ~span list_ty (fun elem_ty ->
                               ty_variant.variants
                               |> Row.await_inferred_to_list
                               |> List.map
                                    (fun
                                        ((variant_label, variant_data) :
                                          Label.t * Types.ty_variant_data)
                                       ->
                                       let name = Label.get_name variant_label in
                                       construct_tuple
                                         ~span
                                         (elem_ty
                                          |> Ty.await_inferred
                                          |> Ty.Shape.expect_tuple
                                          |> Option.unwrap)
                                       <| Tuple.make
                                            []
                                            [ ( "name"
                                              , fun _ ->
                                                  V_String name |> Value.inferred ~span )
                                            ; ( "data"
                                              , fun variant_data_ty ->
                                                  let variant_data_ty =
                                                    variant_data_ty
                                                    |> Ty.await_inferred
                                                    |> Ty.Shape.expect_variant
                                                    |> Option.unwrap
                                                  in
                                                  match variant_data.data with
                                                  | Some data_ty ->
                                                    construct_variant
                                                      ~span
                                                      variant_data_ty
                                                      "Some"
                                                      (Some
                                                         (fun _ ->
                                                           V_Ty data_ty
                                                           |> Value.inferred ~span))
                                                  | None ->
                                                    construct_variant
                                                      ~span
                                                      variant_data_ty
                                                      "None"
                                                      None )
                                            ])) )
                       ]))
         | T_Tuple ty_tuple ->
           construct_variant
             ~span
             type_info_ty
             "Tuple"
             (Some
                (fun data_ty ->
                  let data_ty =
                    data_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
                  in
                  construct_tuple ~span data_ty
                  <| Tuple.make
                       []
                       [ ( "unnamed"
                         , fun list_ty ->
                             construct_slist ~span list_ty (fun _elem_ty ->
                               ty_tuple.tuple.unnamed
                               |> Array.to_list
                               |> List.map (fun (field : Types.ty_tuple_field) ->
                                 V_Ty field.ty |> Value.inferred ~span)) )
                       ; ( "named"
                         , fun list_ty ->
                             construct_slist ~span list_ty (fun elem_ty ->
                               ty_tuple.tuple.named_order_rev
                               |> List.rev
                               |> List.map (fun name ->
                                 name, ty_tuple.tuple.named |> StringMap.find name)
                               |> List.map
                                    (fun
                                        ((name, field) : string * Types.ty_tuple_field) ->
                                       construct_tuple
                                         ~span
                                         (elem_ty
                                          |> Ty.await_inferred
                                          |> Ty.Shape.expect_tuple
                                          |> Option.unwrap)
                                       <| Tuple.make
                                            [ (fun _ ->
                                                V_String name |> Value.inferred ~span)
                                            ; (fun _ ->
                                                V_Ty field.ty |> Value.inferred ~span)
                                            ]
                                            [])) )
                       ]))
         | T_List _ -> failwith __LOC__
         | T_Ty -> failwith __LOC__
         | T_Fn _ -> failwith __LOC__
         | T_Generic _ -> failwith __LOC__
         | T_Ast -> failwith __LOC__
         | T_UnwindToken _ -> failwith __LOC__
         | T_Target -> failwith __LOC__
         | T_ContextTy -> failwith __LOC__
         | T_ImplicitContext -> failwith __LOC__
         | T_CompilerScope -> failwith __LOC__
         | T_Opaque _ -> failwith __LOC__
         | T_Blocked _ -> failwith __LOC__
         | T_Error -> failwith __LOC__)
      | None ->
        Error.error span "reflection.type_info needs type as arg, got %a" Value.print arg;
        V_Error |> Value.inferred ~span)
  ]
;;
