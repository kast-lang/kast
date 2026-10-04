open Common

let init () =
  [ native_fn "syntax.number_literal" (fun _ty ~caller:span ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      match arg |> Value.await_inferred with
      | V_Int32 n ->
        let token : Kast_token.Types.number = { raw = Int32.to_string n } in
        let ast : Ast.t =
          { shape =
              Simple { comments_before = []; token = { shape = Number token; span } }
          ; data = span
          }
          |> Kast_ast_init.init_ast
        in
        V_Ast ast |> Value.inferred ~span
      | _ ->
        Error.error span "syntax.number_literal expected int32 arg";
        V_Error |> Value.inferred ~span)
  ; native_fn "syntax.ident" (fun _ty ~caller:span ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      match arg |> Value.await_inferred with
      | V_String name ->
        let token : Kast_token.Types.ident = { raw = name; name } in
        let ast : Ast.t =
          { shape = Simple { comments_before = []; token = { shape = Ident token; span } }
          ; data = span
          }
          |> Kast_ast_init.init_ast
        in
        V_Ast ast |> Value.inferred ~span
      | _ ->
        Error.error span "syntax.ident expected string arg";
        V_Error |> Value.inferred ~span)
  ; native_fn "Ast.get_comma_separated_list" (fun _ty ~caller:span ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      match arg |> Value.await_inferred with
      | V_Ast ast ->
        let ty : Types.ty_list = { element_ty = T_Ast |> Ty.inferred ~span } in
        let elements =
          ast
          |> Ast.collect_list
               ~trailing_or_leading_rule_name:"core:trailing comma"
               ~binary_rule_name:"core:comma"
        in
        let elements : place Dynarray.t =
          elements
          |> List.map (fun ast ->
            Place.init ~mut:Immutable (V_Ast ast |> Value.inferred ~span))
          |> Dynarray.of_list
        in
        V_List { ty; elements } |> Value.inferred ~span
      | _ ->
        Error.error span "Ast.get_comma_separated_list expected ast arg";
        V_Error |> Value.inferred ~span)
  ; native_fn "Ast.shape" (fun fn_ty ~caller:span ~state:_ arg : value ->
      let shape_ty =
        fn_ty.result |> Ty.await_inferred |> Ty.Shape.expect_variant |> Option.unwrap
      in
      let arg = single_arg ~span arg in
      match arg |> Value.await_inferred with
      | V_Ast ast ->
        (match ast.shape with
         | String { parts; _ } ->
           let make_parts parts_ty =
             let parts_ty =
               parts_ty |> Ty.await_inferred |> Ty.Shape.expect_list |> Option.unwrap
             in
             let part_ty =
               parts_ty.element_ty
               |> Ty.await_inferred
               |> Ty.Shape.expect_variant
               |> Option.unwrap
             in
             construct_list
               ~span
               parts_ty
               (parts
                |> List.map (fun (part : Ast.str_part) ->
                  match part with
                  | Content { contents; _ } ->
                    let content_data content_data_ty =
                      let content_data_ty =
                        content_data_ty
                        |> Ty.await_inferred
                        |> Ty.Shape.expect_tuple
                        |> Option.unwrap
                      in
                      construct_tuple
                        ~span
                        content_data_ty
                        (Tuple.make
                           []
                           [ ( "content"
                             , fun _ -> V_String contents |> Value.inferred ~span )
                           ])
                    in
                    construct_variant ~span part_ty "Content" (Some content_data)
                  | Interpolate { value; _ } ->
                    let interpolate_data interpolate_data_ty =
                      let interpolate_data_ty =
                        interpolate_data_ty
                        |> Ty.await_inferred
                        |> Ty.Shape.expect_tuple
                        |> Option.unwrap
                      in
                      construct_tuple
                        ~span
                        interpolate_data_ty
                        (Tuple.make
                           []
                           [ ("value", fun _ -> V_Ast value.ast |> Value.inferred ~span) ])
                    in
                    construct_variant ~span part_ty "Interpolate" (Some interpolate_data))
               )
           in
           let make_data data_ty =
             let data_ty =
               data_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
             in
             construct_tuple ~span data_ty (Tuple.make [] [ "parts", make_parts ])
           in
           construct_variant ~span shape_ty "String" (Some make_data)
         | _ -> construct_variant ~span shape_ty "Unknown" None)
      | _ ->
        Error.error span "Ast.shape expected ast arg";
        V_Error |> Value.inferred ~span)
  ]
;;
