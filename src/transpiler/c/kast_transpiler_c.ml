open Std
open Kast_types
open Kast_util
module Inference = Kast_inference
module Interpreter = Kast_interpreter
module C_ast = C_ast

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
  ; init_statics : block
  }

type _ Effect.t += CurrentFnCaptured : Types.interpreter_scope Effect.t
type _ Effect.t += GetCtx : ctx Effect.t
type _ Effect.t += GetBindingModuleMap : string Id.Map.t Effect.t
type _ Effect.t += GetCurrentBlock : block Effect.t

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
    let name = gen_name "compound" in
    insert_stmt (DeclareVar { name; ty });
    fields
    |> List.iter (fun (name, value) ->
      insert_stmt (Assign { assignee = Field { obj = Ident name; field = name }; value }));
    Claim (Ident name)

  and new_block (f : unit -> unit) : C_ast.block =
    let block = { stmts = [] } in
    try
      f ();
      block.stmts
    with
    | effect GetCurrentBlock, k -> Effect.continue k block

  and transpile_place_expr (expr : Expr.Place.t) : C_ast.place_expr =
    match expr.shape with
    | Types.PE_Binding binding ->
      (match
         Effect.perform CurrentFnCaptured |> Kast_interpreter.Scope.find_opt binding.name
       with
       (* if I compile a closure, all captured variables are promoted to be consts in generated code *)
       | Some place -> transpile_value (Interpreter.read_place ~span place)
       | None -> Ident (binding_name binding))
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
    Inference.Var.setup_default_if_needed ty.var;
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked value) -> failwith __LOC__ (* Any *)
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
                  (List.print Span.print)
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
    let def : C_ast.ty_def = Enum (StringSet.of_list variant_names) in
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
    let def : C_ast.ty_def = Union (StringMap.of_list variants) in
    ctx.types <- ctx.types |> StringMap.add name def;
    Named name

  and add_include s =
    let ctx = Effect.perform GetCtx in
    ctx.includes <- ctx.includes |> StringSet.add s

  and transpile_ty_shape (ty : Types.ty_shape) : C_ast.ty_def =
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
    | Types.T_String -> failwith __LOC__
    | Types.T_Char ->
      Alias
        (add_include "wchar.h";
         Raw "wchar_t")
    | Types.T_Ref { mut = _; referenced } -> Alias (Ptr (transpile_ty referenced))
    | Types.T_Variant ty ->
      Struct (StringMap.of_list [ "tag", variant_tag_ty ty; "data", variant_data_ty ty ])
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
    | Types.T_Ty -> failwith __LOC__ (* Alias (Ref (Named "TypeInfo")) *)
    | Types.T_Fn { args; result; async = _ } ->
      let args = args.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap in
      let args =
        args.tuple
        |> Tuple.to_seq
        |> Seq.map (fun ((_member, field) : Tuple.member * Types.ty_tuple_field) ->
          transpile_ty field.ty)
        |> List.of_seq
      in
      let result_ty = transpile_ty result in
      Fn { args; result_ty }
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
      failwith __LOC__
    | Types.T_CompilerScope -> Alias Unit
    | Types.T_Opaque _ -> failwith __LOC__
    | Types.T_Blocked _ -> failwith __LOC__
    | Types.T_Error -> fail "transpiling error ty"

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

  and transpile_fn
        ~(captured : Types.interpreter_scope option)
        (def : Types.maybe_compiled_fn)
    : C_ast.fn_def
    =
    let captured =
      captured |> Option.unwrap_or_else (fun () -> Effect.perform CurrentFnCaptured)
    in
    try
      let def =
        Interpreter.await_compiled ~span def
        |> Option.unwrap_or_else (fun () -> fail "fn not compiled")
      in
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
      let result_ty = transpile_ty def.body.data.signature.ty in
      let body : C_ast.block =
        new_block (fun () ->
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
          execute_expr def.body)
      in
      { args; result_ty; body }
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
             | V_Fn { fn = { def; captured; _ }; _ }
             | V_Generic { fn = { def; captured; _ }; _ } ->
               (* TODO memoize generics *)
               let fn_def = transpile_fn ~captured:(Some captured) def in
               ctx.fns <- ctx.fns |> StringMap.add value_name fn_def;
               None
             | _ -> Some (transpile_value_shape shape))
        in
        match value_expr with
        | None -> ()
        | Some value_expr ->
          let static : C_ast.static =
            { name = value_name; ty = transpile_ty (Value.ty_of value) }
          in
          (try
             insert_stmt (Assign { assignee = Ident value_name; value = value_expr })
           with
           | effect GetCurrentBlock, k -> Effect.continue k ctx.init_statics);
          Dynarray.add_last ctx.statics static);
      Ident value_name

  and transpile_value_shape (shape : Types.value_shape) : C_ast.expr =
    match shape with
    | V_Unit -> Unit
    | V_Bool x -> Literal (Bool x)
    | V_Int32 x -> Literal (Int32 x)
    | V_Int64 x -> Literal (Int64 x)
    | V_Float64 x -> Literal (Float64 x)
    | V_Char x -> Literal (Char x)
    | V_Ref _ -> failwith __LOC__
    | V_String s -> failwith __LOC__ (* Literal (String s) *)
    | V_Tuple { ty = _; tuple } ->
      let fields =
        tuple
        |> Tuple.to_seq
        |> Seq.map
             (fun
                 ((member, field) : Tuple.member * Types.value_tuple_field)
                  : (string * C_Ast.expr)
                ->
                ( member_name member
                , C_ast.Claim
                    (transpile_value (field.place |> Interpreter.read_place ~span)) ))
        |> List.of_seq
      in
      compound_literal (transpile_ty (Value.Shape.ty_of shape)) fields
    | V_List _ -> failwith __LOC__
    | V_Variant _ -> failwith __LOC__
    | V_Ty ty -> failwith __LOC__ (* AddrOf (TypeInfo (transpile_ty ty)) *)
    | V_Fn _ | V_Generic _ -> fail "unreachable, we should never compile fns as consts"
    | V_NativeFn f -> fail "transpiling native fn %S at %s" f.name __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ ->
      (* TODO maybe? *)
      Uninitialized
    | V_CompilerScope _ -> Unit
    | V_Opaque _ -> failwith __LOC__
    | V_Blocked _ -> failwith __LOC__
    | V_Error -> failwith __LOC__

  and assign (assignee : Types.assignee_expr) (pure_place_expr : C_ast.place_expr)
    : C_ast.expr
    =
    match assignee.shape with
    | Types.A_Placeholder -> Unit
    | Types.A_Unit -> Unit
    | Types.A_Tuple _ -> failwith __LOC__
    | Types.A_Place place ->
      Assign { assignee = transpile_place_expr place; value = Claim pure_place_expr }
    | Types.A_Let pattern -> pattern_match pattern pure_place_expr
    | Types.A_Error -> failwith __LOC__

  and call_fn ~(args_is_tuple : bool) (f : expr) (arg : expr) : C_ast.expr =
    let f = transpile_expr f in
    let exprs : C_ast.expr Dynarray.t = Dynarray.create () in
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
             Dynarray.add_last
               exprs
               (Let
                  { var
                  ; ty = None
                  ; value =
                      Claim (transpile_value (Interpreter.read_place ~span field.place))
                  });
             args := !args |> Tuple.add name var)
         | E_Tuple { parts; _ } ->
           parts
           |> List.iter (function
             | (Field { label; field; _ } : _ Types.tuple_part_of) ->
               let name = label |> Option.map Label.get_name in
               let var = gen_name "arg" in
               Dynarray.add_last
                 exprs
                 (Let { var; ty = None; value = transpile_expr field });
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
    Dynarray.add_last exprs (Apply { f; args });
    Then (exprs |> Dynarray.to_list)

  and context_ty_name ({ id; ty } : Types.value_context_ty) : string =
    let name = gen_name "context" in
    let ctx = Effect.perform GetCtx in
    ctx.context_names <- ctx.context_names |> Id.Map.add id name;
    let context_ty = transpile_ty ty in
    ctx.contexts <- ctx.contexts |> StringMap.add name context_ty;
    name

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
      | Types.E_Then { list } -> List.fold_left (fun _ e -> eval_expr e) None list
      | Types.E_Stmt { expr } ->
        execute_expr expr;
        None
      | Types.E_Scope { expr } -> transpile_expr expr
      | Types.E_Fn { def; _ } ->
        failwith __LOC__ (* Fn (transpile_fn ~captured:None def) *)
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
        let name = Label.get_name label in
        let value =
          match value with
          | Some value -> transpile_expr value
          | None -> Unit
        in
        compound_literal [ "tag", Variant name; "data", value ]
      | Types.E_Apply { f; arg } -> call_fn ~args_is_tuple:true f arg
      | Types.E_InstantiateGeneric { generic; arg } ->
        call_fn ~args_is_tuple:true generic arg
      | Types.E_Assign { assignee; value } ->
        let value_var = gen_name "value" in
        Then
          [ Let { var = value_var; ty = None; value = Ref (transpile_place_expr value) }
          ; assign assignee (Deref (Claim (Ident value_var)))
          ]
      | Types.E_Ty _ -> Obj []
      | Types.E_Newtype _ -> Obj []
      | Types.E_Native { parts; _ } ->
        let parts =
          parts
          |> List.map (fun (part : Types.expr_native_part) : C_ast.native_expr_part ->
            match part with
            | Raw s -> Raw s
            | Expr e -> Interpolated (transpile_expr e))
        in
        TypeAscribed { expr = Native { parts }; ty = transpile_ty expr.data.signature.ty }
      | Types.E_Module { def; bindings } ->
        let var = gen_name "module" in
        let binding_module_map = ref (Effect.perform GetBindingModuleMap) in
        bindings
        |> List.iter (fun (binding : binding) ->
          binding_module_map := !binding_module_map |> Id.Map.add binding.id var);
        let binding_module_map = !binding_module_map in
        (try
           Then
             [ Let
                 { var
                 ; ty = Some (transpile_ty expr.data.signature.ty)
                 ; value = Uninitialized
                 }
             ; transpile_expr def
             ; Claim (Ident var)
             ]
         with
         | effect GetBindingModuleMap, k -> Effect.continue k binding_module_map)
      | Types.E_UseDotStar { bindings; used } ->
        Then
          (let stmts : C_ast.expr Dynarray.t = Dynarray.create () in
           let used_var = gen_name "used" in
           Dynarray.add_last
             stmts
             (Let { var = used_var; ty = None; value = transpile_expr used });
           let used : C_ast.place_expr = Ident used_var in
           bindings
           |> List.iter (fun (binding : binding) ->
             let stmt : C_ast.expr =
               Let
                 { var = binding_name binding
                 ; ty = None
                 ; value = Claim (Field { obj = used; field = binding.name.name })
                 }
             in
             Dynarray.add_last stmts stmt);
           stmts |> Dynarray.to_list)
      | Types.E_If { cond; then_case; else_case } ->
        If
          { cond = transpile_expr cond
          ; then_case = transpile_expr then_case
          ; else_case = Some (transpile_expr else_case)
          }
      | Types.E_And _ -> failwith __LOC__
      | Types.E_Or _ -> failwith __LOC__
      | Types.E_Match { value; branches } ->
        let value_var = gen_name "matched" in
        let value_ref : C_ast.expr = Ref (transpile_place_expr value) in
        (* TODO panic(not exhaustive) *)
        let non_exhaustive : C_ast.expr = Uninitialized in
        Then
          [ Let { var = value_var; ty = None; value = value_ref }
          ; List.fold_right
              (fun ({ pattern; body } : Types.expr_match_branch) acc : C_ast.expr ->
                 If
                   { cond = does_match pattern (Deref (Claim (Ident value_var)))
                   ; then_case =
                       Then
                         [ pattern_match pattern (Deref (Claim (Ident value_var)))
                         ; transpile_expr body
                         ]
                   ; else_case = Some acc
                   })
              branches
              non_exhaustive
          ]
      | Types.E_QuoteAst _ -> failwith __LOC__
      | Types.E_Loop { body } -> Loop (transpile_expr body)
      | Types.E_Unwindable { token; body } ->
        let token_ident = gen_name "token" in
        Unwindable
          { token_ident
          ; body = Then [ pattern_match token (Ident token_ident); transpile_expr body ]
          }
      | Types.E_Unwind { token; value } ->
        Unwind { token = transpile_expr token; value = transpile_expr value }
      | Types.E_InjectContext { context_ty; value } ->
        InjectContext { name = context_ty_name context_ty; value = transpile_expr value }
      | Types.E_CurrentContext { context_ty } ->
        Claim (CurrentContext (context_ty_name context_ty))
      | Types.E_ImplCast _ -> failwith __LOC__
      | Types.E_Cast { value; target } ->
        Cast { value = transpile_expr value; target = cast_target target }
      | Types.E_TargetDependent target_dependent ->
        let branch =
          Kast_interpreter.find_target_dependent_branch
            ctx.interpreter
            target_dependent
            ctx.target
        in
        (match branch with
         | Some branch -> transpile_expr branch.body
         | None -> fail "no C cfg branch at %a" Span.print expr.data.span)
      | Types.E_Error -> fail "transpiling error expr"
    with
    | Cancel -> raise Cancel
    | e ->
      Log.error (fun log -> log "while transpiling expr at %a" Span.print span);
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
    }
  in
  let current_fn_captured = Interpreter.Scope.init ~recursive:false ~parent:None ~span in
  let main : C_ast.fn_def =
    try
      let body = Impl.transpile_expr expr in
      { args = []
      ; result_ty = Impl.transpile_ty expr.data.signature.ty
      ; body = Scope body
      }
    with
    | effect CurrentFnCaptured, k -> Effect.continue k current_fn_captured
    | effect GetCtx, k -> Effect.continue k ctx
    | effect GetBindingModuleMap, k -> Effect.continue k Id.Map.empty
  in
  ctx.fns <- ctx.fns |> StringMap.add "main" main;
  { types = ctx.types
  ; includes = ctx.includes
  ; fns = ctx.fns
  ; statics = ctx.statics |> Dynarray.to_list
  }
;;
