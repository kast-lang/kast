open Common

let init () =
  [ native_fn "syntax.number_literal" (fun _ty ~caller:span ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      match arg |> Value.await_inferred with
      | V_Int32 n ->
        let token : Kast_token.Shape.number = { raw = Int32.to_string n } in
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
        let token : Kast_token.Shape.ident = { raw = name; name } in
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
  ]
;;
