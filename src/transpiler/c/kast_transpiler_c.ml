open Std
open Kast_types
open Kast_util
module Inference = Kast_inference
module Interpreter = Kast_interpreter
module C_ast = C_ast

let print_span = Span.print

type block = { mutable stmts : C_ast.stmt list }

type ctx =
  { interpreter : Kast_interpreter.state
  ; target : Types.value_target
  ; mutable includes : StringSet.t
  ; mutable types : C_ast.ty_def StringMap.t
  ; mutable fns : C_ast.fn_def StringMap.t
  ; mutable statics : C_ast.static Dynarray.t
  ; mutable captured_values : string Types.ValueMap.t
  ; mutable captured_types : string Types.ValueMap.t
  ; mutable contexts : Types.value_context_ty Id.Map.t
  ; init_statics : block
  }

type current_captured =
  { interpreter_scope : Interpreter.Scope.t
  ; bindings : C_ast.place_expr Id.Map.t
  ; interpreter_state : Interpreter.state
  }

type unwind_ctx =
  { mutable insert_unwind : unit -> unit
  ; mutable cleanup_scope_without_unwind : unit -> unit
  }

type _ Effect.t += CurrentFnCaptured : current_captured Effect.t
type _ Effect.t += GetCtx : ctx Effect.t
type _ Effect.t += GetBindingModuleMap : string Id.Map.t Effect.t
type _ Effect.t += GetCurrentBlock : block Effect.t
type _ Effect.t += GetScopeCtxPtr : C_ast.expr Effect.t
type _ Effect.t += GetUnwindCtx : unwind_ctx Effect.t

type transpiled_fn =
  { captured_ty_name : string option
  ; def : Types.compiled_fn
  ; name : string
  }

let span = Span.of_ocaml __POS__

let c_keywords =
  StringSet.of_list
    [ "alignas"
    ; "alignof"
    ; "auto"
    ; "bool"
    ; "break"
    ; "case"
    ; "char"
    ; "const"
    ; "constexpr"
    ; "continue"
    ; "default"
    ; "do"
    ; "double"
    ; "else"
    ; "enum"
    ; "extern"
    ; "false"
    ; "float"
    ; "for"
    ; "goto"
    ; "if"
    ; "inline"
    ; "int"
    ; "long"
    ; "nullptr"
    ; "register"
    ; "restrict"
    ; "return"
    ; "short"
    ; "signed"
    ; "sizeof"
    ; "static"
    ; "static_assert"
    ; "struct"
    ; "switch"
    ; "thread_local"
    ; "true"
    ; "typedef"
    ; "typeof"
    ; "typeof_unqual"
    ; "union"
    ; "unsigned"
    ; "void"
    ; "volatile"
    ; "while"
    ; "_Alignas"
    ; "_Alignof"
    ; "_Atomic"
    ; "_BitInt"
    ; "_Bool"
    ; "_Complex"
    ; "_Decimal128"
    ; "_Decimal32"
    ; "_Decimal64"
    ; "_Generic"
    ; "_Imaginary"
    ; "_Noreturn"
    ; "_Static_assert"
    ; "_Thread_local"
    ]
;;

module Impl = struct
  let rec _unused () = ()

  and binding_name (binding : binding) : string =
    make_correct_ident (make_string "%s_%a" binding.name.name Id.print binding.id)

  and member_name (member : Tuple.member) : string =
    match member with
    | Index i -> "_" ^ Int.to_string i
    | Name name -> make_correct_ident name

  and variant_tag_name (variant_ty : ty) (label : Label.t) : string =
    match transpile_ty variant_ty with
    | Named name -> make_correct_ident (name ^ "_" ^ Label.get_name label)
    | _ -> failwith __LOC__

  and insert_stmt (stmt : C_ast.stmt) : unit =
    let block = Effect.perform GetCurrentBlock in
    block.stmts <- block.stmts @ [ stmt ]

  and let_var (ty : C_ast.ty) (name : string) (value : C_ast.expr) : unit =
    insert_stmt (DeclareVar { name; ty });
    insert_stmt (Assign { assignee = Ident name; value })

  and compound_literal (ty : C_ast.ty) (fields : (string * C_ast.expr) list) : C_ast.expr =
    let result_name = gen_name "compound" in
    insert_stmt (DeclareVar { name = result_name; ty });
    fields
    |> List.iter (fun (name, value) ->
      insert_stmt
        (Assign { assignee = Field { obj = Ident result_name; field = name }; value }));
    Claim (Ident result_name)

  and new_block (f : unit -> unit) : C_ast.block =
    let block = { stmts = [] } in
    try
      f ();
      block.stmts
    with
    | effect GetCurrentBlock, k -> Effect.continue k block

  and lookup_binding (binding : binding) : C_ast.place_expr =
    let captured = Effect.perform CurrentFnCaptured in
    match captured.interpreter_scope |> Kast_interpreter.Scope.find_opt binding.name with
    (* if I compile a closure, all captured variables are promoted to be consts in generated code *)
    | Some place -> transpile_value (Interpreter.read_place ~span place)
    | None ->
      (match captured.bindings |> Id.Map.find_opt binding.id with
       | Some place -> place
       | None -> Ident (binding_name binding))

  and transpile_place_expr (expr : Expr.Place.t) : C_ast.place_expr =
    match expr.shape with
    | Types.PE_Binding binding -> lookup_binding binding
    | Types.PE_Field { obj; field; field_span = _ } ->
      let field =
        match field with
        | Types.Index i -> member_name (Index i)
        | Types.Name label -> member_name (Name (Label.get_name label))
        | Types.Expr _ -> failwith __LOC__
      in
      Field { obj = transpile_place_expr obj; field }
    | Types.PE_Deref expr -> Deref (transpile_expr expr)
    | Types.PE_Temp expr -> Temp (transpile_expr expr)
    | Types.PE_Error -> fail "transpiling error place expr"

  and transpile_ty (ty : ty) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let ty =
      Interpreter.Substitute_bindings.sub_ty
        ~span
        ~state:(Interpreter.sub_here (Effect.perform CurrentFnCaptured).interpreter_state)
        ty
    in
    Inference.Var.setup_default_if_needed ty.var;
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked value) -> Ptr Void
    | _ ->
      let ty_name = ref None in
      let do_prepend = ref false in
      ctx.captured_types
      <- ctx.captured_types
         |> Types.ValueMap.update
              (V_Ty ty |> Value.inferred ~span)
              (fun name ->
                 let name =
                   match name with
                   | Some name -> name
                   | None ->
                     let name = gen_name ~opt:(Ty.name ty) "type" in
                     do_prepend := true;
                     name
                 in
                 ty_name := Some name;
                 Some name);
      let ty_name = !ty_name |> Option.get in
      if !do_prepend
      then (
        Log.trace (fun log -> log "prepend %a" Ty.print ty);
        let mini_ty =
          match ty.var |> Inference.Var.inferred_opt with
          | None ->
            if not !Kast_util.quiet
            then
              Log.error (fun log ->
                log
                  "transpiling not inferred ty at %a"
                  (List.print print_span)
                  (Inference.Var.spans ty.var |> SpanSet.to_list));
            failwith __LOC__
          | Some shape -> transpile_ty_shape shape
        in
        ctx.types <- ctx.types |> StringMap.add ty_name mini_ty);
      Named ty_name

  and variant_tag_ty (ty : Types.ty_variant) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let name = gen_name "tag" in
    let variant_names =
      ty.variants
      |> Row.await_inferred_to_list
      |> List.map (fun (label, _) -> make_correct_ident (Label.get_name label))
    in
    let def : C_ast.ty_def =
      { shape = Enum (StringSet.of_list variant_names)
      ; comment = Some (make_string "Tag for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    Named name

  and variant_data_ty (ty : Types.ty_variant) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let name = gen_name "data" in
    let variants =
      ty.variants
      |> Row.await_inferred_to_list
      |> List.map (fun ((label, { data }) : Label.t * Types.ty_variant_data) ->
        let name = make_correct_ident (Label.get_name label) in
        let ty =
          match data with
          | Some ty -> transpile_ty ty
          | None -> Unit
        in
        name, ty)
    in
    let def : C_ast.ty_def =
      { shape = Union (StringMap.of_list variants)
      ; comment = Some (make_string "Data for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    Named name

  and add_include s =
    let ctx = Effect.perform GetCtx in
    ctx.includes <- ctx.includes |> StringSet.add s

  and transpile_ty_shape (ty : Types.ty_shape) : C_ast.ty_def =
    let shape : C_ast.ty_def_shape =
      match ty with
      | Types.T_Unit -> Alias Unit
      | Types.T_Bool ->
        Alias
          (add_include "stdbool.h";
           Raw "bool")
      | Types.T_Int32 ->
        Alias
          (add_include "stdint.h";
           Raw "int32_t")
      | Types.T_Int64 ->
        Alias
          (add_include "stdint.h";
           Raw "int64_t")
      | Types.T_Float64 -> Alias (Raw "double")
      | Types.T_String -> Alias (Raw "String")
      | Types.T_Char ->
        Alias
          (add_include "wchar.h";
           Raw "wchar_t")
      | Types.T_Ref { mut = _; referenced } -> Alias (Ptr (transpile_ty referenced))
      | Types.T_Variant ty ->
        Struct
          (StringMap.of_list [ "tag", variant_tag_ty ty; "data", variant_data_ty ty ])
      | Types.T_Tuple { name = _; tuple } ->
        let fields =
          tuple
          |> Tuple.to_seq
          |> Seq.map
               (fun
                   ((member, field) : Tuple.member * Types.ty_tuple_field)
                    : (string * C_ast.ty)
                  -> member_name member, transpile_ty field.ty)
          |> StringMap.of_seq
        in
        Struct fields
      | Types.T_List { element_ty } -> failwith __LOC__
      | Types.T_Ty -> Alias Unit (* Alias (Ref (Named "TypeInfo")) *)
      | Types.T_Fn { is_closure; call_convention; args; result; async = _ } ->
        let args =
          args.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
        in
        let args =
          args.tuple
          |> Tuple.to_seq
          |> Seq.map (fun ((_member, field) : Tuple.member * Types.ty_tuple_field) ->
            transpile_ty field.ty)
          |> List.of_seq
        in
        let args = if is_closure then [ C_ast.Ptr Void ] @ args else args in
        let args =
          match call_convention with
          | None -> [ C_ast.Ptr (Raw "Context") ] @ args
          | Some "C" -> args
          | _ -> fail "unknown call convention"
        in
        let result_ty = transpile_ty result in
        (match is_closure with
         | true ->
           let raw_fn_ty = gen_name "raw_fn_ty" in
           let ctx = Effect.perform GetCtx in
           ctx.types
           <- ctx.types
              |> StringMap.add
                   raw_fn_ty
                   ({ shape = Fn { args; result_ty }
                    ; comment =
                        Some (make_string "raw fn type for %a" Print.print_ty_shape ty)
                    }
                    : C_ast.ty_def);
           Struct (StringMap.of_list [ "captured", C_ast.Ptr Void; "f", Named raw_fn_ty ])
         | false -> Fn { args; result_ty })
      | Types.T_Generic _ when true -> Alias Unit
      | Types.T_Generic { args; result } ->
        let args =
          args.pattern.data.signature.ty
          |> Ty.await_inferred
          |> Ty.Shape.expect_tuple
          |> Option.unwrap
        in
        let args =
          args.tuple
          |> Tuple.to_seq
          |> Seq.map (fun ((_member, field) : Tuple.member * Types.ty_tuple_field) ->
            transpile_ty field.ty)
          |> List.of_seq
        in
        let result_ty = transpile_ty result in
        Fn { args; result_ty }
      | Types.T_Ast -> failwith __LOC__
      | Types.T_UnwindToken { result } -> failwith __LOC__
      | Types.T_Target -> failwith __LOC__
      | Types.T_ContextTy ->
        (* TODO maybe? *)
        Alias Unit
      | Types.T_CompilerScope -> Alias Unit
      | Types.T_Opaque _ -> failwith __LOC__
      | Types.T_Blocked _ -> failwith __LOC__
      | Types.T_Error -> fail "transpiling error ty"
    in
    { shape; comment = Some (make_string "%a" Print.print_ty_shape ty) }

  and does_match (pattern : pattern) (pure_place_expr : C_ast.place_expr) : C_ast.expr =
    match pattern.shape with
    | Types.P_Placeholder -> Literal (Bool true)
    | Types.P_Ref { mut = _; referenced } ->
      does_match referenced (Deref (Claim pure_place_expr))
    | Types.P_Unit -> Literal (Bool true)
    | Types.P_Binding _ -> Literal (Bool true)
    | Types.P_Tuple { parts; _ } ->
      let unnamed_idx = ref 0 in
      let had_unpack = ref false in
      parts
      |> List.map (fun (part : pattern Types.tuple_part_of) : C_ast.expr ->
        match part with
        | Field { label; field; _ } ->
          let member =
            match label with
            | None ->
              if !had_unpack then failwith __LOC__;
              let member = Tuple.Member.Index !unnamed_idx in
              unnamed_idx := !unnamed_idx + 1;
              member
            | Some label -> Tuple.Member.Name (Label.get_name label)
          in
          does_match field (Field { obj = pure_place_expr; field = member_name member })
        | Unpack pattern ->
          (match pattern.shape with
           | P_Placeholder -> ()
           | _ -> failwith __LOC__);
          had_unpack := true;
          Literal (Bool true))
      |> List.fold_left (fun a b : C_ast.expr -> And (a, b)) (Literal (Bool true))
    | Types.P_Variant { label; _ } ->
      Equal
        ( Claim (Field { obj = pure_place_expr; field = "tag" })
        , Claim (Ident (variant_tag_name pattern.data.signature.ty label)) )
    | Types.P_Error -> failwith __LOC__

  and pattern_match (pattern : pattern) (pure_place_expr : C_ast.place_expr) : unit =
    match pattern.shape with
    | Types.P_Placeholder -> ()
    | Types.P_Ref { mut = _; referenced } ->
      pattern_match referenced (Deref (Claim pure_place_expr))
    | Types.P_Unit -> ()
    | Types.P_Binding { bind_mode; binding } ->
      let value : C_ast.expr =
        match bind_mode with
        | Types.Claim -> Claim pure_place_expr
        | Types.ByRef { mut = _ } -> AddrOf pure_place_expr
      in
      (match Effect.perform GetBindingModuleMap |> Id.Map.find_opt binding.id with
       | Some module_name ->
         insert_stmt
           (Assign
              { assignee = Field { obj = Ident module_name; field = binding.name.name }
              ; value
              })
       | None ->
         insert_stmt
           (DeclareVar { name = binding_name binding; ty = transpile_ty binding.ty });
         insert_stmt (Assign { assignee = Ident (binding_name binding); value }))
    | Types.P_Tuple { parts; _ } ->
      let unnamed_idx = ref 0 in
      let had_unpack = ref false in
      parts
      |> List.iter (fun (part : pattern Types.tuple_part_of) ->
        match part with
        | Field { label; field; _ } ->
          let member =
            match label with
            | None ->
              if !had_unpack then failwith __LOC__;
              let member = Tuple.Member.Index !unnamed_idx in
              unnamed_idx := !unnamed_idx + 1;
              member
            | Some label -> Tuple.Member.Name (Label.get_name label)
          in
          pattern_match
            field
            (Field { obj = pure_place_expr; field = member_name member })
        | Unpack pattern ->
          (match pattern.shape with
           | P_Placeholder -> ()
           | _ -> failwith __LOC__);
          had_unpack := true)
    | Types.P_Variant { label; value = data; _ } ->
      (match data with
       | Some data_pattern ->
         pattern_match data_pattern (Field { obj = pure_place_expr; field = "data" })
       | None -> ())
    | Types.P_Error -> failwith __LOC__

  and defer (f : unit -> unit) =
    let unwind_ctx = Effect.perform GetUnwindCtx in
    let old_cleanup_scope_without_unwind = unwind_ctx.cleanup_scope_without_unwind in
    unwind_ctx.cleanup_scope_without_unwind
    <- (fun () ->
         f ();
         old_cleanup_scope_without_unwind ())

  and transpile_fn
        ~(is_closure : bool)
        ~(call_convention : string option)
        ~(captured : Types.interpreter_scope option)
        (def : Types.maybe_compiled_fn)
    : transpiled_fn
    =
    let ctx = Effect.perform GetCtx in
    let def_span = def.span in
    let def =
      Interpreter.await_compiled ~span def
      |> Option.unwrap_or_else (fun () -> fail "fn not compiled")
    in
    let captured_interpreter_scope : Interpreter.Scope.t =
      captured
      |> Option.unwrap_or_else (fun () ->
        (Effect.perform CurrentFnCaptured).interpreter_scope)
    in
    let captured_interpreter =
      { ctx.interpreter with
        scope =
          Interpreter.Scope.init
            ~span:def.body.data.span
            ~recursive:false
            ~parent:(Some captured_interpreter_scope)
      ; monomorphization_state = Types.init_monomorphization_state ()
      }
    in
    let captured_arg_name = gen_name "captured_void" in
    let typed_captured_arg_name = gen_name "captured" in
    let captured_bindings =
      def.captures
      |> List.filter (fun (binding : binding) ->
        captured_interpreter_scope
        |> Interpreter.Scope.find_opt binding.name
        |> Option.is_none)
    in
    let captured_ty_name, captured_bindings =
      match captured_bindings with
      | [] -> None, Id.Map.empty
      | captures ->
        let name = gen_name "captured" in
        let def : C_ast.ty_def =
          { shape =
              Struct
                (captures
                 |> List.map (fun (binding : binding) ->
                   binding_name binding, C_ast.Ptr (transpile_ty binding.ty))
                 |> StringMap.of_list)
          ; comment = Some (make_string "captured of closure at %a" print_span def_span)
          }
        in
        ctx.types <- ctx.types |> StringMap.add name def;
        ( Some name
        , captures
          |> List.map (fun (binding : binding) : (Id.t * C_ast.place_expr) ->
            ( binding.id
            , Deref
                (Claim
                   (Field
                      { obj = Deref (Claim (Ident typed_captured_arg_name))
                      ; field = binding_name binding
                      })) ))
          |> Id.Map.of_list )
    in
    let captured : current_captured =
      { interpreter_scope = captured_interpreter_scope
      ; bindings = captured_bindings
      ; interpreter_state = captured_interpreter
      }
    in
    try
      let result_ty = transpile_ty def.body.data.signature.ty in
      let unwind_ctx : unwind_ctx =
        { insert_unwind =
            (fun () ->
              let var = gen_name "unwind_return" in
              insert_stmt (DeclareVar { name = var; ty = result_ty });
              insert_stmt (Return (Claim (Ident var))))
        ; cleanup_scope_without_unwind = (fun () -> ())
        }
      in
      try
        let args =
          match def.args.pattern.data.signature.ty |> Ty.await_inferred with
          | T_Tuple { name = _; tuple } ->
            tuple
            |> Tuple.to_seq
            |> Seq.map
                 (fun
                     ((member, field) : Tuple.member * Types.ty_tuple_field)
                      : C_ast.fn_arg
                    -> { name = member_name member; ty = transpile_ty field.ty })
            |> List.of_seq
          | _ -> fail "fn args are not tuple"
        in
        let args =
          if is_closure
          then [ ({ name = captured_arg_name; ty = Ptr Void } : C_ast.fn_arg) ] @ args
          else args
        in
        let ctx_var = gen_name "ctx" in
        let args =
          match call_convention with
          | None ->
            [ ({ name = ctx_var; ty = Ptr (Raw "Context") } : C_ast.fn_arg) ] @ args
          | Some "C" -> args
          | Some other -> fail "unknown call convension %S" other
        in
        let body : C_ast.block =
          new_block (fun () ->
            try
              def.captures
              |> List.iter (fun (binding : binding) ->
                insert_stmt (Comment (make_string "captured %a" Binding.print binding)));
              (match captured_ty_name with
               | Some captured_ty_name ->
                 let_var
                   (Ptr (Named captured_ty_name))
                   typed_captured_arg_name
                   (Claim (Ident captured_arg_name))
               | None -> ());
              let arg_parts =
                match def.args.pattern.shape with
                | P_Tuple { parts; _ } -> parts
                | _ -> fail "fn args must be tuple"
              in
              let unnamed_idx = ref 0 in
              arg_parts
              |> List.iter (fun (part : pattern Types.tuple_part_of) ->
                match part with
                | Field { label; field; _ } ->
                  let member =
                    match label with
                    | None ->
                      let result = Tuple.Member.Index !unnamed_idx in
                      unnamed_idx := !unnamed_idx + 1;
                      result
                    | Some label -> Tuple.Member.Name (Label.get_name label)
                  in
                  pattern_match field (Ident (member_name member))
                | Unpack _ -> failwith __LOC__);
              unwind_ctx.cleanup_scope_without_unwind ();
              match eval_expr def.body with
              | None -> ()
              | Some result -> insert_stmt (Return result)
            with
            | effect GetScopeCtxPtr, k -> Effect.continue k (Claim (Ident ctx_var)))
        in
        let name = gen_name "fn" in
        ctx.fns
        <- ctx.fns
           |> StringMap.add
                name
                ({ args
                 ; result_ty
                 ; body
                 ; comment = Some (make_string "fn at %a" print_span def_span)
                 }
                 : C_ast.fn_def);
        { captured_ty_name; name; def }
      with
      | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx
    with
    | effect CurrentFnCaptured, k -> Effect.continue k captured

  and make_correct_ident (name : string) : string =
    let result = ref "" in
    if c_keywords |> StringSet.contains name then result := "_KAST_";
    name
    |> String.iter (fun c ->
      if !result = "" && Char.is_digit c then result := "_";
      let c = if Char.is_alphanumeric c || c = '_' then c else '_' in
      result := !result ^ String.make 1 c);
    !result

  and gen_name ?opt:optional_name (name : string) : string =
    let name =
      match optional_name |> Option.and_then OptionalName.await_inferred with
      | Some name -> make_string "%a" Print.print_name_shape name
      | None -> name
    in
    let name = make_correct_ident name in
    make_string "%s%d" name (Id.gen ()).value

  and not_inferred (var : _ Inference.var) : C_ast.expr = failwith __LOC__

  and transpile_value (value : value) : C_ast.place_expr =
    let ctx = Effect.perform GetCtx in
    Inference.Var.setup_default_if_needed value.var;
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked value) -> failwith __LOC__
    | _ ->
      (try
         let value_name = ref None in
         let do_prepend = ref false in
         ctx.captured_values
         <- ctx.captured_values
            |> Types.ValueMap.update value (fun name ->
              let name =
                match name with
                | Some name -> name
                | None ->
                  let name = gen_name ~opt:(Value.name value) "const" in
                  do_prepend := true;
                  name
              in
              value_name := Some name;
              Some name);
         let value_name = !value_name |> Option.get in
         if !do_prepend
         then (
           Log.trace (fun log -> log "prepend %a" Value.print value);
           let value_expr =
             match value.var |> Inference.Var.inferred_opt with
             | None -> Some (not_inferred value.var)
             | Some shape ->
               (match shape with
                | V_Fn { fn = { def; captured; _ }; ty = fn_ty } ->
                  let f =
                    transpile_fn
                      ~call_convention:fn_ty.call_convention
                      ~is_closure:fn_ty.is_closure
                      ~captured:(Some captured)
                      def
                  in
                  if fn_ty.is_closure && f.captured_ty_name |> Option.is_none
                  then
                    Some
                      (Block
                         (new_block (fun () ->
                            let var = gen_name "fn_as_closure" in
                            insert_stmt
                              (DeclareVar
                                 { name = var; ty = transpile_ty (Value.ty_of value) });
                            insert_stmt
                              (Assign
                                 { assignee =
                                     Field { obj = Ident var; field = "captured" }
                                 ; value = Native { parts = [ Raw "NULL" ] }
                                 });
                            insert_stmt
                              (Assign
                                 { assignee = Field { obj = Ident var; field = "f" }
                                 ; value = Claim (Ident f.name)
                                 });
                            insert_stmt (Expr (Claim (Ident var))))))
                  else Some (Claim (Ident f.name))
                | V_Generic _ when true -> Some Unit
                | V_Generic { fn = { def; captured; _ }; _ } ->
                  (* TODO memoize generics *)
                  let f =
                    transpile_fn
                      ~call_convention:(Some "C")
                      ~is_closure:false
                      ~captured:(Some captured)
                      def
                  in
                  Some (Claim (Ident f.name))
                | _ -> Some (transpile_value_shape shape))
           in
           match value_expr with
           | None -> ()
           | Some value_expr ->
             let static : C_ast.static =
               { name = value_name
               ; ty = transpile_ty (Value.ty_of value)
               ; comment = Some (make_string "%a" Value.print value)
               }
             in
             insert_stmt (Assign { assignee = Ident value_name; value = value_expr });
             Dynarray.add_last ctx.statics static);
         Ident value_name
       with
       | effect GetCurrentBlock, k -> Effect.continue k ctx.init_statics)

  and transpile_value_shape (shape : Types.value_shape) : C_ast.expr =
    match shape with
    | V_Unit -> Unit
    | V_Bool x -> Literal (Bool x)
    | V_Int32 x -> Literal (Int32 x)
    | V_Int64 x -> Literal (Int64 x)
    | V_Float64 x -> Literal (Float64 x)
    | V_Char x -> Literal (Char x)
    | V_Ref _ -> failwith __LOC__
    | V_String s ->
      Apply
        { f = Native { parts = [ Raw "String_from_C_String" ] }
        ; args = [ Literal (String s) ]
        }
    | V_Tuple { ty = _; tuple } ->
      let fields =
        tuple
        |> Tuple.to_seq
        |> Seq.map
             (fun
                 ((member, field) : Tuple.member * Types.value_tuple_field)
                  : (string * C_ast.expr)
                ->
                ( member_name member
                , C_ast.Claim
                    (transpile_value (field.place |> Interpreter.read_place ~span)) ))
        |> List.of_seq
      in
      compound_literal (transpile_ty (Value.Shape.ty_of shape)) fields
    | V_List _ -> failwith __LOC__
    | V_Variant _ -> failwith __LOC__
    | V_Ty ty -> Unit (* AddrOf (TypeInfo (transpile_ty ty)) *)
    | V_Fn _ | V_Generic _ -> fail "unreachable, we should never compile fns as consts"
    | V_NativeFn f -> fail "transpiling native fn %S at %s" f.name __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ ->
      (* TODO maybe? *)
      Unit
    | V_CompilerScope _ -> Unit
    | V_Opaque _ -> failwith __LOC__
    | V_Blocked _ -> failwith __LOC__
    | V_Error -> failwith __LOC__

  and assign (assignee : Types.assignee_expr) (pure_place_expr : C_ast.place_expr) : unit =
    match assignee.shape with
    | Types.A_Placeholder -> ()
    | Types.A_Unit -> ()
    | Types.A_Tuple _ -> failwith __LOC__
    | Types.A_Place place ->
      insert_stmt
        (Assign { assignee = transpile_place_expr place; value = Claim pure_place_expr })
    | Types.A_Let pattern -> pattern_match pattern pure_place_expr
    | Types.A_Error -> failwith __LOC__

  and call_fn ~(args_is_tuple : bool) (f_expr : expr) (arg : expr) : C_ast.expr =
    let ~is_closure, ~call_convention =
      match f_expr.data.signature.ty |> Ty.await_inferred with
      | T_Fn ty -> ~is_closure:ty.is_closure, ~call_convention:ty.call_convention
      | T_Generic _ -> ~is_closure:false, ~call_convention:(Some "C")
      | _ -> fail "Expected fn, got %a" Ty.print f_expr.data.signature.ty
    in
    let f = transpile_expr f_expr in
    let args =
      if args_is_tuple
      then (
        let args = ref Tuple.empty in
        (match arg.shape with
         | E_Constant { value; _ } ->
           let value_tuple =
             value
             |> Value.expect_tuple
             |> Option.unwrap_or_else (fun () -> fail "f args must be tuple")
           in
           value_tuple.tuple
           |> Tuple.iter (fun member (field : Types.value_tuple_field) ->
             let name =
               match member with
               | Index _ -> None
               | Name name -> Some name
             in
             let var = gen_name "arg" in
             let_var
               (transpile_ty field.ty_field.ty)
               var
               (Claim (transpile_value (Interpreter.read_place ~span field.place)));
             args := !args |> Tuple.add name var)
         | E_Tuple { parts; _ } ->
           parts
           |> List.iter (function
             | (Field { label; field : expr; _ } : _ Types.tuple_part_of) ->
               let name = label |> Option.map Label.get_name in
               let var = gen_name "arg" in
               let_var (transpile_ty field.data.signature.ty) var (transpile_expr field);
               args := !args |> Tuple.add name var
             | Unpack _ -> failwith __LOC__)
         | _ -> fail "f args must be tuple");
        let args_ty =
          arg.data.signature.ty
          |> Ty.await_inferred
          |> Ty.Shape.expect_tuple
          |> Option.unwrap
        in
        args_ty.tuple
        |> Tuple.to_seq
        |> Seq.map
             (fun
                 ((member, _arg_ty) : Tuple.member * Types.ty_tuple_field) : C_ast.expr ->
                Claim (Ident (!args |> Tuple.get member)))
        |> List.of_seq)
      else [ transpile_expr arg ]
    in
    let f, args =
      if is_closure
      then (
        let f_name = gen_name "f" in
        let_var (transpile_ty f_expr.data.signature.ty) f_name f;
        ( C_ast.Claim (Field { obj = Ident f_name; field = "f" })
        , [ C_ast.Claim (Field { obj = Ident f_name; field = "captured" }) ] @ args ))
      else f, args
    in
    let args =
      match call_convention with
      | None -> [ Effect.perform GetScopeCtxPtr ] @ args
      | Some "C" -> args
      | Some other -> fail "unknown conv %S" other
    in
    Apply { f; args }

  and context_ty_name ({ id; ty } : Types.value_context_ty) : string = failwith __LOC__
  (* let name = gen_name "context" in *)
  (* let ctx = Effect.perform GetCtx in *)
  (* ctx.context_names <- ctx.context_names |> Id.Map.add id name; *)
  (* let context_ty = transpile_ty ty in *)
  (* ctx.contexts <- ctx.contexts |> StringMap.add name context_ty; *)
  (* name *)

  and construct_pattern_value_with_bindings (pattern : pattern) : value =
    match pattern.shape with
    | Types.P_Placeholder -> Value.new_not_inferred ~scope:None ~span
    | Types.P_Ref { mut; referenced } ->
      V_Ref
        { mut
        ; place =
            Place.init ~mut:Inherit (construct_pattern_value_with_bindings referenced)
        }
      |> Value.inferred ~span
    | Types.P_Unit -> V_Unit |> Value.inferred ~span
    | Types.P_Binding { bind_mode; binding } ->
      V_Blocked
        { shape =
            (match bind_mode with
             | Claim -> BV_Binding binding
             | ByRef { mut } -> failwith __LOC__)
        ; ty = pattern.data.signature.ty
        }
      |> Value.inferred ~span
    | Types.P_Tuple { parts; _ } ->
      let tuple = ref Tuple.empty in
      let ty =
        pattern.data.signature.ty
        |> Ty.await_inferred
        |> Ty.Shape.expect_tuple
        |> Option.unwrap
      in
      let unnamed_idx = ref 0 in
      parts
      |> List.iter (fun (part : pattern Types.tuple_part_of) ->
        match part with
        | Field { label; field; _ } ->
          let ty_field, name =
            match label with
            | None ->
              let ty_field = ty.tuple |> Tuple.get_unnamed !unnamed_idx in
              unnamed_idx := !unnamed_idx + 1;
              ty_field, None
            | Some name ->
              let name = Label.get_name name in
              ty.tuple |> Tuple.get_named name, Some name
          in
          let field_value = construct_pattern_value_with_bindings field in
          let tuple_field : Types.value_tuple_field =
            { place = Place.init ~mut:Inherit field_value; span; ty_field }
          in
          tuple := !tuple |> Tuple.add name tuple_field
        | Unpack _ -> failwith __LOC__);
      V_Tuple { tuple = !tuple; ty } |> Value.inferred ~span
    | Types.P_Variant _ -> failwith __LOC__
    | Types.P_Error -> failwith __LOC__

  and cast_target (target : value) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    match target |> Value.await_inferred with
    | V_Ty _ -> failwith __LOC__
    | V_Generic { ty; _ } ->
      let arg = construct_pattern_value_with_bindings ty.args.pattern in
      let result = Interpreter.instantiate span ctx.interpreter target arg in
      let result_ty = result |> Value.expect_ty |> Option.unwrap in
      transpile_ty result_ty
    | _ -> failwith __LOC__

  and transpile_expr (expr : expr) : C_ast.expr =
    match eval_expr expr with
    | Some e -> e
    | None -> Unit

  and context_field_name (context_ty : Types.value_context_ty) : string =
    make_string "context_%d" context_ty.id.value

  and current_context (context_ty : Types.value_context_ty) : C_ast.place_expr =
    let ctx = Effect.perform GetCtx in
    ctx.contexts <- ctx.contexts |> Id.Map.add context_ty.id context_ty;
    let ctx_ptr = Effect.perform GetScopeCtxPtr in
    Field { obj = Deref ctx_ptr; field = context_field_name context_ty }

  and execute_expr (expr : expr) : unit =
    match eval_expr expr with
    | None -> ()
    | Some e -> insert_stmt (Expr e)

  and eval_expr (expr : expr) : C_ast.expr option =
    let ctx = Effect.perform GetCtx in
    let span = expr.data.span in
    try
      match expr.shape with
      | Types.E_Constant { id = _; value } -> Some (Claim (transpile_value value))
      | Types.E_Ref { mut = _; place } -> Some (AddrOf (transpile_place_expr place))
      | Types.E_Claim place -> Some (Claim (transpile_place_expr place))
      | Types.E_Then { list } ->
        List.fold_left
          (fun acc e ->
             (match acc with
              | None -> ()
              | Some prev -> insert_stmt (Expr prev));
             eval_expr e)
          None
          list
      | Types.E_Stmt { expr } ->
        execute_expr expr;
        None
      | Types.E_Scope { expr } ->
        let parent_unwind_ctx = Effect.perform GetUnwindCtx in
        let unwind_ctx : unwind_ctx =
          { insert_unwind = parent_unwind_ctx.insert_unwind
          ; cleanup_scope_without_unwind = (fun () -> ())
          }
        in
        (try
           let result = eval_expr expr in
           unwind_ctx.cleanup_scope_without_unwind ();
           result
         with
         | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx)
      | Types.E_Fn { ty = f_ty; def; _ } ->
        if f_ty.is_closure
        then (
          let transpiled_fn =
            transpile_fn
              ~call_convention:f_ty.call_convention
              ~is_closure:f_ty.is_closure
              ~captured:None
              def
          in
          let result_var = gen_name "closure" in
          insert_stmt
            (DeclareVar { name = result_var; ty = transpile_ty expr.data.signature.ty });
          let captured_arg : C_ast.expr =
            match transpiled_fn.captured_ty_name with
            | None -> Native { parts = [ Raw "NULL" ] }
            | Some captured_ty_name ->
              let captured_var_name = gen_name "captured" in
              insert_stmt
                (DeclareVar { name = captured_var_name; ty = Named captured_ty_name });
              transpiled_fn.def.captures
              |> List.iter (fun (binding : binding) ->
                insert_stmt
                  (Assign
                     { assignee =
                         Field
                           { obj = Ident captured_var_name; field = binding_name binding }
                     ; value = AddrOf (lookup_binding binding)
                     }));
              AddrOf (Ident captured_var_name)
          in
          insert_stmt
            (Assign
               { assignee = Field { obj = Ident result_var; field = "captured" }
               ; value = captured_arg
               });
          insert_stmt
            (Assign
               { assignee = Field { obj = Ident result_var; field = "f" }
               ; value = Claim (Ident transpiled_fn.name)
               });
          Some (Claim (Ident result_var)))
        else (
          let transpiled_fn =
            transpile_fn
              ~call_convention:f_ty.call_convention
              ~is_closure:f_ty.is_closure
              ~captured:None
              def
          in
          Some (Claim (Ident transpiled_fn.name)))
      | Types.E_Generic { def; _ } ->
        (* TODO memoization? *)
        failwith __LOC__
        (* Fn (transpile_fn ~captured:None def) *)
      | Types.E_Tuple { guaranteed_anonymous = _; parts } ->
        let var_name = gen_name "tuple" in
        insert_stmt
          (DeclareVar { name = var_name; ty = transpile_ty expr.data.signature.ty });
        let unnamed_idx = ref 0 in
        parts
        |> List.iter (fun (part : expr Types.tuple_part_of) ->
          match part with
          | Field { label; field; _ } ->
            let member =
              match label with
              | None ->
                let member = Tuple.Member.Index !unnamed_idx in
                unnamed_idx := !unnamed_idx + 1;
                member
              | Some label -> Tuple.Member.Name (Label.get_name label)
            in
            let field_name = member_name member in
            insert_stmt
              (Assign
                 { assignee = Field { obj = Ident var_name; field = field_name }
                 ; value = transpile_expr field
                 })
          | Unpack packed ->
            let packed_ty =
              packed.data.signature.ty
              |> Ty.await_inferred
              |> Ty.Shape.expect_tuple
              |> Option.unwrap
            in
            let packed_name = gen_name "packed" in
            let_var
              (transpile_ty packed.data.signature.ty)
              packed_name
              (transpile_expr packed);
            packed_ty.tuple
            |> Tuple.iter (fun member (field : Types.ty_tuple_field) ->
              let assignee_member =
                match member with
                | Index i ->
                  let member = Tuple.Member.Index !unnamed_idx in
                  unnamed_idx := !unnamed_idx + 1;
                  member
                | Name name -> Name name
              in
              insert_stmt
                (Assign
                   { assignee =
                       Field { obj = Ident var_name; field = member_name assignee_member }
                   ; value =
                       Claim
                         (Field { obj = Ident packed_name; field = member_name member })
                   })));
        Some (Claim (Ident var_name))
      | Types.E_Variant { label; value; _ } ->
        let value =
          match value with
          | Some value -> transpile_expr value
          | None -> Unit
        in
        Some
          (compound_literal
             (transpile_ty expr.data.signature.ty)
             [ "tag", Claim (Ident (variant_tag_name expr.data.signature.ty label))
             ; "data", value
             ])
      | Types.E_Apply { f; arg } -> Some (call_fn ~args_is_tuple:true f arg)
      | Types.E_InstantiateGeneric { generic; arg } ->
        Some (call_fn ~args_is_tuple:true generic arg)
      | Types.E_Assign { assignee; value } ->
        let value_var = gen_name "value" in
        assign assignee (transpile_place_expr value);
        None
      | Types.E_Ty _ -> failwith __LOC__
      | Types.E_Newtype _ -> Some Unit
      | Types.E_Native { parts; _ } ->
        with_return (fun { return } : C_ast.expr option ->
          (match parts with
           | [ Raw "#unreachable" ] -> return None
           | [ Raw s ] ->
             (match s |> String.strip_prefix ~prefix:"#include <" with
              | None -> ()
              | Some s ->
                (match s |> String.strip_suffix ~suffix:">" with
                 | None -> ()
                 | Some s ->
                   ctx.includes <- ctx.includes |> StringSet.add s;
                   return None))
           | _ -> ());
          let parts =
            parts
            |> List.map (fun (part : Types.expr_native_part) : C_ast.native_expr_part ->
              match part with
              | Raw s -> Raw s
              | Expr e -> Interpolated (transpile_expr e))
          in
          Some (Native { parts }))
      | Types.E_Module { def; bindings } ->
        let var = gen_name "module" in
        let binding_module_map = ref (Effect.perform GetBindingModuleMap) in
        bindings
        |> List.iter (fun (binding : binding) ->
          binding_module_map := !binding_module_map |> Id.Map.add binding.id var);
        let binding_module_map = !binding_module_map in
        (try
           insert_stmt
             (DeclareVar { name = var; ty = transpile_ty expr.data.signature.ty });
           execute_expr def;
           Some (Claim (Ident var))
         with
         | effect GetBindingModuleMap, k -> Effect.continue k binding_module_map)
      | Types.E_UseDotStar { bindings; used } ->
        (* TODO not needed? *)
        (* let stmts : C_ast.expr Dynarray.t = Dynarray.create () in *)
        (* let used_var = gen_name "used" in *)
        (* let_var (transpile_ty used.data.signature.ty) used_var (transpile_expr used); *)
        (* let used : C_ast.place_expr = Ident used_var in *)
        (* bindings *)
        (* |> List.iter (fun (binding : binding) -> *)
        (*   let_var *)
        (*     (transpile_ty binding.ty) *)
        (*     (binding_name binding) *)
        (*     (Claim (Field { obj = used; field = binding.name.name }))); *)
        None
      | Types.E_If { cond; then_case; else_case } ->
        let cond = transpile_expr cond in
        let var = gen_name "if_result" in
        let then_case =
          new_block (fun () ->
            insert_stmt
              (Assign { assignee = Ident var; value = transpile_expr then_case }))
        in
        let else_case =
          new_block (fun () ->
            insert_stmt
              (Assign { assignee = Ident var; value = transpile_expr else_case }))
        in
        insert_stmt (DeclareVar { name = var; ty = transpile_ty expr.data.signature.ty });
        insert_stmt (If { cond; then_case; else_case = Some else_case });
        Some (Claim (Ident var))
      | Types.E_And _ -> failwith __LOC__
      | Types.E_Or _ -> failwith __LOC__
      | Types.E_Match { value; branches } ->
        let value_var = gen_name "matched" in
        (* TODO panic(not exhaustive) *)
        let_var
          (Ptr (transpile_ty value.data.signature.ty))
          value_var
          (AddrOf (transpile_place_expr value));
        let result_var = gen_name "match_result" in
        let end_of_match_label = gen_name "end_of_match" in
        insert_stmt
          (DeclareVar { name = result_var; ty = transpile_ty expr.data.signature.ty });
        branches
        |> List.iter (fun ({ pattern; body } : Types.expr_match_branch) ->
          insert_stmt
            (If
               { cond = does_match pattern (Deref (Claim (Ident value_var)))
               ; then_case =
                   new_block (fun () ->
                     pattern_match pattern (Deref (Claim (Ident value_var)));
                     insert_stmt
                       (Assign
                          { assignee = Ident result_var; value = transpile_expr body });
                     insert_stmt (Goto { label = end_of_match_label }))
               ; else_case = None
               }));
        insert_stmt (GotoLabel end_of_match_label);
        Some (Claim (Ident result_var))
      | Types.E_QuoteAst _ -> failwith __LOC__
      | Types.E_Loop { body } ->
        insert_stmt (For { body = new_block (fun () -> execute_expr body) });
        None
      | Types.E_Unwindable { token; body } ->
        failwith __LOC__
        (* let token_ident = gen_name "token" in *)
        (* Unwindable *)
        (*   { token_ident *)
        (*   ; body = Then [ pattern_match token (Ident token_ident); transpile_expr body ] *)
        (*   } *)
      | Types.E_Unwind { token; value } ->
        failwith __LOC__
        (* Unwind { token = transpile_expr token; value = transpile_expr value } *)
      | Types.E_InjectContext { context_ty; value } ->
        let value = transpile_expr value in
        let old_var = gen_name "old_ctx" in
        let ctx_place = current_context context_ty in
        let_var (transpile_ty context_ty.ty) old_var (Claim ctx_place);
        insert_stmt (Assign { assignee = ctx_place; value });
        defer (fun () ->
          insert_stmt (Assign { assignee = ctx_place; value = Claim (Ident old_var) }));
        None
      | Types.E_CurrentContext { context_ty } -> Some (Claim (current_context context_ty))
      | Types.E_ImplCast _ -> failwith __LOC__
      | Types.E_Cast { value; target } ->
        failwith __LOC__
        (* Cast { value = transpile_expr value; target = cast_target target } *)
      | Types.E_TargetDependent target_dependent ->
        let branch =
          Kast_interpreter.find_target_dependent_branch
            ctx.interpreter
            target_dependent
            ctx.target
        in
        (match branch with
         | Some branch -> eval_expr branch.body
         | None -> fail "no C cfg branch at %a" print_span expr.data.span)
      | Types.E_Error -> fail "transpiling error expr"
    with
    | Cancel -> raise Cancel
    | e ->
      Log.error (fun log -> log "while transpiling expr at %a" print_span span);
      raise e
  ;;
end

let transpile_expr (interpreter : Interpreter.state) (expr : expr) : C_ast.program =
  let ctx : ctx =
    { interpreter
    ; target = { name = "c" }
    ; statics = Dynarray.create ()
    ; captured_values = Types.ValueMap.empty
    ; captured_types = Types.ValueMap.empty
    ; types = StringMap.empty
    ; fns = StringMap.empty
    ; init_statics = { stmts = [] }
    ; includes = StringSet.empty
    ; contexts = Id.Map.empty
    }
  in
  let captured_scope = Interpreter.Scope.init ~recursive:false ~parent:None ~span in
  let captured : current_captured =
    { interpreter_scope = captured_scope
    ; bindings = Id.Map.empty
    ; interpreter_state =
        { ctx.interpreter with
          scope =
            Interpreter.Scope.init
              ~span:expr.data.span
              ~recursive:false
              ~parent:(Some captured_scope)
        ; monomorphization_state = Types.init_monomorphization_state ()
        }
    }
  in
  let ctx_var = Impl.gen_name "ctx" in
  let context_ty_def : C_ast.ty_def option ref = ref None in
  let unwind_ctx : unwind_ctx =
    { insert_unwind =
        (fun () -> Impl.insert_stmt (Return (Literal (Int32 (Int32.of_int (-1))))))
    ; cleanup_scope_without_unwind = (fun () -> ())
    }
  in
  (try
     let main : C_ast.fn_def =
       { args = []
       ; result_ty = Raw "int"
       ; body =
           Impl.new_block (fun () ->
             Impl.insert_stmt (DeclareVar { name = ctx_var; ty = Raw "Context" });
             Impl.insert_stmt
               (Expr (Apply { f = Claim (Ident "KAST_init_statics"); args = [] }));
             Impl.execute_expr expr;
             unwind_ctx.cleanup_scope_without_unwind ();
             Impl.insert_stmt (Return (Literal (Int32 (Int32.of_int 0)))))
       ; comment = None
       }
     in
     let init_statics : C_ast.fn_def =
       { args = []
       ; result_ty = Raw "void"
       ; body = ctx.init_statics.stmts
       ; comment = None
       }
     in
     ctx.fns <- ctx.fns |> StringMap.add "main" main;
     ctx.fns <- ctx.fns |> StringMap.add "KAST_init_statics" init_statics;
     context_ty_def
     := Some
          ({ shape =
               Struct
                 (ctx.contexts
                  |> Id.Map.to_list
                  |> List.map (fun ((_id, context_ty) : Id.t * Types.value_context_ty) ->
                    Impl.context_field_name context_ty, Impl.transpile_ty context_ty.ty)
                  |> StringMap.of_list)
           ; comment = Some "implicit context"
           }
           : C_ast.ty_def)
   with
   | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx
   | effect GetScopeCtxPtr, k -> Effect.continue k (AddrOf (Ident ctx_var))
   | effect CurrentFnCaptured, k -> Effect.continue k captured
   | effect GetCtx, k -> Effect.continue k ctx
   | effect GetBindingModuleMap, k -> Effect.continue k Id.Map.empty);
  { types = ctx.types |> StringMap.add "Context" (!context_ty_def |> Option.unwrap)
  ; includes = ctx.includes
  ; fns = ctx.fns
  ; statics = ctx.statics |> Dynarray.to_list
  }
;;
