open Std
open Kast_types
open Kast_util
module Inference = Kast_inference
module Interpreter = Kast_interpreter
module MiniAst = Minikast_ast

type ctx =
  { interpreter : Kast_interpreter.state
  ; target : Types.value_target
  ; mutable types : MiniAst.ty_def StringMap.t
  ; mutable fns : MiniAst.fn_def StringMap.t
  ; mutable consts : MiniAst.expr StringMap.t
  ; mutable captured_values : string Types.ValueMap.t
  }

type _ Effect.t += GetCtx : ctx Effect.t

module Impl = struct
  let span = Span.of_ocaml __POS__

  let rec _unused () = ()
  and binding_name (binding : binding) : string = binding.name.name

  and member_name (member : Tuple.member) : string =
    match member with
    | Index i -> "_" ^ Int.to_string i
    | Name name -> name

  and transpile_place_expr (expr : Expr.Place.t) : MiniAst.place_expr =
    match expr.shape with
    | Types.PE_Binding binding -> Ident (binding_name binding)
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

  and transpile_ty (ty : ty) : MiniAst.ty =
    match ty |> Ty.await_inferred with
    | Types.T_Unit -> Unit
    | Types.T_Bool -> Bool
    | Types.T_Int32 -> Int32
    | Types.T_Int64 -> failwith __LOC__
    | Types.T_Float64 -> failwith __LOC__
    | Types.T_String -> String
    | Types.T_Char -> Char
    | Types.T_Ref { mut = _; referenced } -> Ref (transpile_ty referenced)
    | Types.T_Variant _ -> failwith __LOC__
    | Types.T_Tuple _ -> failwith __LOC__
    | Types.T_List _ -> failwith __LOC__
    | Types.T_Ty -> failwith __LOC__
    | Types.T_Fn { args; result; async = _ } ->
      let args = args.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap in
      let args =
        args.tuple
        |> Tuple.to_seq
        |> Seq.map (fun ((_member, field) : Tuple.member * Types.ty_tuple_field) ->
          transpile_ty field.ty)
        |> List.of_seq
      in
      let result = transpile_ty result in
      Fn { args; result }
    | Types.T_Generic _ -> failwith __LOC__
    | Types.T_Ast -> failwith __LOC__
    | Types.T_UnwindToken _ -> failwith __LOC__
    | Types.T_Target -> failwith __LOC__
    | Types.T_ContextTy -> failwith __LOC__
    | Types.T_CompilerScope -> failwith __LOC__
    | Types.T_Opaque _ -> failwith __LOC__
    | Types.T_Blocked _ -> failwith __LOC__
    | Types.T_Error -> fail "transpiling error ty"

  and pattern_match (pattern : pattern) (place : MiniAst.place_expr) : MiniAst.expr =
    match pattern.shape with
    | Types.P_Placeholder -> Unit
    | Types.P_Ref _ -> failwith __LOC__
    | Types.P_Unit -> Unit
    | Types.P_Binding { bind_mode; binding } ->
      let value : MiniAst.expr =
        match bind_mode with
        | Types.Claim -> Claim place
        | Types.ByRef { mut = _ } -> Ref place
      in
      Let { var = binding_name binding; value }
    | Types.P_Tuple _ -> failwith __LOC__
    | Types.P_Variant _ -> failwith __LOC__
    | Types.P_Error -> failwith __LOC__

  and transpile_fn_expr (def : Types.maybe_compiled_fn) : MiniAst.expr =
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
                  : MiniAst.fn_arg
                -> { name = member_name member; ty = transpile_ty field.ty })
        |> List.of_seq
      | _ -> fail "fn args are not tuple"
    in
    let result_ty = transpile_ty def.body.data.signature.ty in
    let body : MiniAst.expr =
      Scope
        (let body = Dynarray.create () in
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
             Dynarray.add_last body (pattern_match field (Ident (member_name member)))
           | Unpack _ -> failwith __LOC__);
         Dynarray.add_last body (transpile_expr def.body);
         Then (Dynarray.to_list body))
    in
    Fn { args; result_ty; body }

  and gen_name (name : string) : string = make_string "%s%d" name (Id.gen ()).value
  and not_inferred (var : _ Inference.var) : MiniAst.expr = Uninitialized

  and transpile_value (value : value) : MiniAst.expr =
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
               let name = gen_name "const" in
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
          | None -> not_inferred value.var
          | Some shape -> transpile_value_shape shape
        in
        ctx.consts <- ctx.consts |> StringMap.add value_name value_expr);
      Claim (Ident value_name)

  and transpile_value_shape (shape : Types.value_shape) : MiniAst.expr =
    match shape with
    | V_Unit -> Unit
    | V_Bool x -> Bool x
    | V_Int32 _ -> failwith __LOC__
    | V_Int64 _ -> failwith __LOC__
    | V_Float64 _ -> failwith __LOC__
    | V_Char _ -> failwith __LOC__
    | V_Ref _ -> failwith __LOC__
    | V_String s -> String s
    | V_Tuple _ -> failwith __LOC__
    | V_List _ -> failwith __LOC__
    | V_Variant _ -> failwith __LOC__
    | V_Ty _ -> failwith __LOC__
    | V_Fn { fn = { def; _ }; _ } -> transpile_fn_expr def
    | V_Generic _ -> failwith __LOC__
    | V_NativeFn _ -> failwith __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ -> failwith __LOC__
    | V_CompilerScope _ -> failwith __LOC__
    | V_Opaque _ -> failwith __LOC__
    | V_Blocked _ -> failwith __LOC__
    | V_Error -> failwith __LOC__

  and transpile_expr (expr : expr) : MiniAst.expr =
    let ctx = Effect.perform GetCtx in
    let span = expr.data.span in
    try
      match expr.shape with
      | Types.E_Constant { id = _; value } -> transpile_value value
      | Types.E_Ref { mut = _; place } -> Ref (transpile_place_expr place)
      | Types.E_Claim place -> Claim (transpile_place_expr place)
      | Types.E_Then { list } -> Then (list |> List.map transpile_expr)
      | Types.E_Stmt { expr } -> Stmt (transpile_expr expr)
      | Types.E_Scope { expr } -> Scope (transpile_expr expr)
      | Types.E_Fn { def; _ } -> transpile_fn_expr def
      | Types.E_Generic { def; _ } ->
        (* TODO memoization? *)
        transpile_fn_expr def
      | Types.E_Tuple _ -> failwith __LOC__
      | Types.E_Variant _ -> failwith __LOC__
      | Types.E_Apply { f; arg } ->
        let f = transpile_expr f in
        let exprs : MiniAst.expr Dynarray.t = Dynarray.create () in
        let args = ref Tuple.empty in
        (match arg.shape with
         | E_Tuple { parts; _ } ->
           parts
           |> List.iter (function
             | (Field { label; field; _ } : _ Types.tuple_part_of) ->
               let name = label |> Option.map Label.get_name in
               let var = gen_name "arg" in
               Dynarray.add_last exprs (Let { var; value = transpile_expr field });
               args := !args |> Tuple.add name var
             | Unpack _ -> failwith __LOC__)
         | _ -> fail "f args must be tuple");
        let args_ty =
          arg.data.signature.ty
          |> Ty.await_inferred
          |> Ty.Shape.expect_tuple
          |> Option.unwrap
        in
        let args =
          args_ty.tuple
          |> Tuple.to_seq
          |> Seq.map
               (fun
                   ((member, _arg_ty) : Tuple.member * Types.ty_tuple_field)
                    : MiniAst.expr
                  -> Claim (Ident (!args |> Tuple.get member)))
          |> List.of_seq
        in
        Dynarray.add_last exprs (Apply { f; args });
        Then (exprs |> Dynarray.to_list)
      | Types.E_InstantiateGeneric _ -> failwith __LOC__
      | Types.E_Assign _ -> failwith __LOC__
      | Types.E_Ty _ -> failwith __LOC__
      | Types.E_Newtype _ -> failwith __LOC__
      | Types.E_Native { parts; _ } ->
        let parts =
          parts
          |> List.map (fun (part : Types.expr_native_part) : MiniAst.native_expr_part ->
            match part with
            | Raw s -> Raw s
            | Expr e -> Interpolated (transpile_expr e))
        in
        Native { parts }
      | Types.E_Module _ -> failwith __LOC__
      | Types.E_UseDotStar { bindings; used } ->
        Then
          (let stmts : MiniAst.expr Dynarray.t = Dynarray.create () in
           let used_var = gen_name "used" in
           Dynarray.add_last stmts (Let { var = used_var; value = transpile_expr used });
           let used : MiniAst.place_expr = Ident used_var in
           bindings
           |> List.iter (fun (binding : binding) ->
             let stmt : MiniAst.expr =
               Let
                 { var = binding_name binding
                 ; value = Claim (Field { obj = used; field = binding.name.name })
                 }
             in
             Dynarray.add_last stmts stmt);
           stmts |> Dynarray.to_list)
      | Types.E_If _ -> failwith __LOC__
      | Types.E_And _ -> failwith __LOC__
      | Types.E_Or _ -> failwith __LOC__
      | Types.E_Match _ -> failwith __LOC__
      | Types.E_QuoteAst _ -> failwith __LOC__
      | Types.E_Loop _ -> failwith __LOC__
      | Types.E_Unwindable _ -> failwith __LOC__
      | Types.E_Unwind _ -> failwith __LOC__
      | Types.E_InjectContext _ -> failwith __LOC__
      | Types.E_CurrentContext _ -> failwith __LOC__
      | Types.E_ImplCast _ -> failwith __LOC__
      | Types.E_Cast _ -> failwith __LOC__
      | Types.E_TargetDependent target_dependent ->
        let branch =
          Kast_interpreter.find_target_dependent_branch
            ctx.interpreter
            target_dependent
            ctx.target
        in
        (match branch with
         | Some branch -> transpile_expr branch.body
         | None -> fail "no js cfg branch at %a" Span.print expr.data.span)
      | Types.E_Error -> fail "transpiling error expr"
    with
    | Cancel -> raise Cancel
    | e ->
      Log.error (fun log -> log "while transpiling expr at %a" Span.print span);
      raise e
  ;;
end

let transpile_expr
      (target : Types.value_target)
      (interpreter : Interpreter.state)
      (expr : expr)
  : MiniAst.program
  =
  let ctx : ctx =
    { interpreter
    ; target
    ; consts = StringMap.empty
    ; captured_values = Types.ValueMap.empty
    ; types = StringMap.empty
    ; fns = StringMap.empty
    }
  in
  let main : MiniAst.fn_def =
    try
      let body = Impl.transpile_expr expr in
      { args = []
      ; result_ty = Impl.transpile_ty expr.data.signature.ty
      ; body = Scope body
      }
    with
    | effect GetCtx, k -> Effect.continue k ctx
  in
  ctx.fns <- ctx.fns |> StringMap.add "main" main;
  { types = ctx.types; fns = ctx.fns; consts = ctx.consts }
;;
