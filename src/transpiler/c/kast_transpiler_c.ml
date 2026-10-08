open Std
open Kast_types
open Kast_util
module Inference = Kast_inference
module Interpreter = Kast_interpreter
module C_ast = C_ast
module ValueMap = Types.ValueMap.CompareMap

module CTyMap = Map.Make (struct
    type t = C_ast.ty

    let compare = C_ast.compare_ty
  end)

let print_span = Span.print
let debug = ref false

type gc_mode =
  | EscapeAnalyze
  | RuntimeBorrowChecker
  | Disabled

let gc_mode = ref Disabled
let allocation_stats = ref false
let boxed_structs = ref false

let tuple_place_to_data_place place : C_ast.place_expr =
  if !boxed_structs then P_Deref (Pure_Copy place) else place
;;

let tuple_place var : C_ast.place_expr = tuple_place_to_data_place (P_Ident var)

type block = { mutable stmts : C_ast.stmt list }

module StringListMap = Map.Make (struct
    type t = string list

    let compare = List.compare String.compare
  end)

type transpiled_ty_shape =
  | Alias of C_ast.ty
  | T_Named of
      { name : string
      ; def : unit -> C_ast.ty_def
      }

type type_info =
  { type_info_name : string
  ; kast_ty : ty
  }

type 'a progress =
  | Inprogress
  | Completed of 'a

type ctx =
  { target : Types.value_target
  ; mutable includes : StringSet.t
  ; mutable types : C_ast.ty_def StringMap.t
  ; mutable type_infos : type_info CTyMap.t
  ; raw_type_infos : (unit -> unit) Dynarray.t
  ; mutable fns : C_ast.fn_def StringMap.t
  ; mutable statics : C_ast.static Dynarray.t
  ; mutable captured_values : string ValueMap.t
  ; mutable captured_types : C_ast.ty progress ValueMap.t
  ; mutable drop_fns : string ValueMap.t
  ; mutable dbg_write_fns : string ValueMap.t
  ; mutable claim_fns : string ValueMap.t
  ; mutable contexts : Types.value_context_ty Id.Map.t
  ; runtime_defined_closure_types : string StringListMap.t
  ; runtime_defined_list_types : string StringMap.t
  ; runtime_defined_box_types : string StringMap.t
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

type scope = { mutable ctx_place : C_ast.place_expr }
type _ Effect.t += GetInterpreter : Interpreter.state Effect.t
type _ Effect.t += CurrentFnCaptured : current_captured Effect.t
type _ Effect.t += GetCtx : ctx Effect.t
type _ Effect.t += GetBindingModuleMap : string Id.Map.t Effect.t
type _ Effect.t += GetCurrentBlock : block Effect.t
type _ Effect.t += GetScope : scope Effect.t
type _ Effect.t += GetUnwindCtx : unwind_ctx Effect.t

type transpiled_fn =
  { captured_ty_name : string option
  ; captured : binding Id.Map.t
  ; is_move : bool
  ; def : Types.compiled_fn
  ; name : string
  }

let span = Span.of_ocaml __POS__
let emscripten_keywords = [ "stdin"; "stdout"; "stderr" ]

let c_keywords =
  StringSet.of_list
    (emscripten_keywords
     @ [ "alignas"
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
       ])
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
    | T_Named name -> variant_tag_name_impl name label
    | _ -> failwith __LOC__

  and variant_tag_name_impl (ty_name : string) (label : Label.t) : string =
    make_correct_ident (ty_name ^ "_" ^ Label.get_name label)

  and insert_stmt (stmt : C_ast.stmt) : unit =
    let block = Effect.perform GetCurrentBlock in
    block.stmts <- block.stmts @ [ stmt ]

  and ty_to_string (ty : C_ast.ty) : string =
    let ctx = Effect.perform GetCtx in
    match ty with
    | T_Unit -> "Unit"
    | T_Raw { c; is_primitive = _ } -> c
    | T_Named name ->
      name
      (* (match ctx.types |> StringMap.find_opt name with *)
      (*  | Some { shape = Alias ty; _ } -> ty_to_string ty *)
      (*  | Some _ -> name *)
      (*  | None -> fail "type %S is named but not in ctx.types" name) *)
    | T_Ptr p ->
      let name = ty_to_string p ^ "_Ptr" in
      let ty_def : C_ast.ty_def = { shape = TD_Alias ty; comment = None } in
      ctx.types <- ctx.types |> StringMap.add name ty_def;
      name
    | T_Void -> "void"

  and type_info_name_for (kast_ty : ty) : string =
    let ty = transpile_ty kast_ty in
    let ctx = Effect.perform GetCtx in
    match ctx.type_infos |> CTyMap.find_opt ty with
    | Some info -> info.type_info_name
    | None ->
      let name = gen_name (make_string "%s_TypeInfo" (ty_to_string ty)) in
      let info : type_info = { type_info_name = name; kast_ty } in
      ctx.type_infos <- ctx.type_infos |> CTyMap.add ty info;
      name

  and malloc_typed_ptr ~(boxed : bool) (kast_ty : ty) (ptr : C_ast.place_expr) : unit =
    let ty = transpile_ty kast_ty in
    let ty =
      if boxed
      then (
        match ty with
        | T_Ptr ty -> ty
        | _ -> failwith __LOC__)
      else ty
    in
    let ty_info_name = type_info_name_for kast_ty in
    insert_stmt
      (S_Assign
         { assignee = ptr
         ; value = E_Native [ N_Raw ("Kast_allocate(&" ^ ty_info_name ^ ")") ]
         })

  and make_pure (ty : C_ast.ty) (expr : C_ast.expr) (temp_var_name_prefix : string)
    : C_ast.pure_expr
    =
    match expr with
    | E_Pure expr -> expr
    | _ ->
      let name = gen_name temp_var_name_prefix in
      insert_stmt (S_Comment "pure");
      let_c_var ty name (Some expr);
      Pure_Copy (P_Ident name)

  and insert_drop (kast_ty : ty) (value : C_ast.expr) : unit =
    let value = make_pure (transpile_ty kast_ty) value "to_drop" in
    insert_stmt
      (S_Expr
         (E_Apply { f = Pure_Copy (P_Ident (generate_drop kast_ty)); args = [ value ] }))

  and let_var
        ?(drop : bool = true)
        (kast_ty : ty)
        (name : string)
        (value : C_ast.expr option)
    : unit
    =
    let ty = transpile_ty kast_ty in
    insert_stmt (S_DeclareVar { name; ty; value });
    if drop then defer (fun () -> insert_drop kast_ty (E_Pure (Pure_Copy (P_Ident name))))

  and let_c_var (ty : C_ast.ty) (name : string) (value : C_ast.expr option) : unit =
    insert_stmt (S_DeclareVar { name; ty; value })

  and c_compound_literal (ty : C_ast.ty) (fields : (string * C_ast.expr) list)
    : C_ast.expr
    =
    let rec all_fields_pure (fields : (string * C_ast.expr) list)
      : C_ast.field_initializer list option
      =
      match fields with
      | [] -> Some []
      | (name, E_Pure value) :: rest ->
        let* rest = all_fields_pure rest in
        Some (({ name; value } : C_ast.field_initializer) :: rest)
      | _ -> None
    in
    match all_fields_pure fields with
    | Some fields -> E_Pure (Pure_Compound { ty; fields })
    | None ->
      let result_name = gen_name "compound" in
      let_c_var ty result_name None;
      fields
      |> List.iter (fun (name, value) ->
        insert_stmt
          (S_Assign
             { assignee =
                 P_Field
                   { obj =
                       (if !boxed_structs
                        then P_Deref (Pure_Copy (P_Ident result_name))
                        else P_Ident result_name)
                   ; field = name
                   }
             ; value
             }));
      E_Pure (Pure_Copy (P_Ident result_name))

  and compound_literal (kast_ty : ty) (fields : (string * C_ast.expr) list) : C_ast.expr =
    c_compound_literal (transpile_ty kast_ty) fields

  and with_new_scope : 'a. (unit -> 'a) -> 'a =
    fun f ->
    let parent_unwind_ctx = Effect.perform GetUnwindCtx in
    let unwind_ctx : unwind_ctx =
      { insert_unwind = parent_unwind_ctx.insert_unwind
      ; cleanup_scope_without_unwind = (fun () -> ())
      }
    in
    let parent_scope = Effect.perform GetScope in
    let scope : scope = { ctx_place = parent_scope.ctx_place } in
    try
      let result = f () in
      unwind_ctx.cleanup_scope_without_unwind ();
      result
    with
    | effect GetScope, k -> Effect.continue k scope
    | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx

  and new_block ?(after_cleanup : (unit -> unit) option) (f : unit -> unit) : C_ast.block =
    let block = { stmts = [] } in
    try
      with_new_scope f;
      (match after_cleanup with
       | None -> ()
       | Some f -> f ());
      block.stmts
    with
    | effect GetCurrentBlock, k -> Effect.continue k block

  and let_binding (binding : binding) (value : C_ast.expr) : unit =
    match Effect.perform GetBindingModuleMap |> Id.Map.find_opt binding.id with
    | Some _ -> insert_stmt (S_Assign { assignee = lookup_binding binding; value })
    | None ->
      let ident = binding_name binding in
      insert_stmt (S_Comment (make_string "let %s" (binding_name binding)));
      let_var binding.ty ident (Some value)

  and ident_place (name : string) : C_ast.place_expr = get_actual_place (P_Ident name)

  and get_actual_place (place : C_ast.place_expr) : C_ast.place_expr =
    match !gc_mode with
    | _ -> place

  and lookup_binding (binding : binding) : C_ast.place_expr =
    let captured = Effect.perform CurrentFnCaptured in
    match captured.interpreter_scope |> Kast_interpreter.Scope.find_opt binding.name with
    (* if I compile a closure, all captured variables are promoted to be consts in generated code *)
    | Some place -> transpile_value (Interpreter.read_place ~span place)
    | None ->
      (match captured.bindings |> Id.Map.find_opt binding.id with
       | Some place -> place
       | None ->
         (match Effect.perform GetBindingModuleMap |> Id.Map.find_opt binding.id with
          | Some module_name ->
            P_Field
              { obj = get_actual_place (tuple_place module_name)
              ; field = binding.name.name
              }
          | None -> ident_place (binding_name binding)))

  and transpile_place_expr (expr : Expr.Place.t) : C_ast.place_expr =
    match place_expr_const_propagate expr with
    | Some place -> transpile_place place
    | None ->
      (match expr.shape with
       | Types.PE_Binding binding -> lookup_binding binding
       | Types.PE_Const place -> transpile_place place
       | Types.PE_Context -> (Effect.perform GetScope).ctx_place
       | Types.PE_CurrentContext { context_ty } -> current_context context_ty
       | Types.PE_Scope expr ->
         (match expr.shape with
          | PE_Temp expr ->
            let var = gen_name "temp" in
            let value = eval_scoped_expr expr in
            let_var
              expr.data.signature.ty
              var
              (Some
                 (match value with
                  | None -> E_Pure Pure_Unit
                  | Some value -> value));
            P_Ident var
          | _ -> transpile_place_expr expr)
       | Types.PE_Field { obj; field; field_span = _ } ->
         let field =
           match field with
           | Types.Index i -> member_name (Index i)
           | Types.Name label -> member_name (Name (Label.get_name label))
           | Types.Expr _ -> failwith __LOC__
         in
         let obj = transpile_place_expr obj in
         P_Field
           { obj = (if !boxed_structs then P_Deref (Pure_Copy obj) else obj); field }
       | Types.PE_Deref expr ->
         (match expr.data.signature.ty |> Ty.await_inferred with
          | T_Box boxed_ty ->
            P_Deref
              (Pure_Native
                 [ N_Raw "Box_"
                 ; N_Raw (ty_to_string (transpile_ty boxed_ty))
                 ; N_Raw "_deref("
                 ; N_Interpolated (Pure_AddrOf (transpile_place_expr expr))
                 ; N_Raw ")"
                 ])
          | _ -> P_Deref (Pure_Copy (transpile_place_expr expr)))
       | Types.PE_Temp expr ->
         let var = gen_name "temp" in
         let_var expr.data.signature.ty var (Some (transpile_expr expr));
         P_Ident var
       | Types.PE_Error -> fail "transpiling error place expr")

  and place_expr_const_propagate (expr : Expr.Place.t) : place option =
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    match expr.shape with
    | Types.PE_Const place -> Some place
    | Types.PE_Field { obj; field; field_span = _ } ->
      let* obj = place_expr_const_propagate obj in
      let member : Tuple.member =
        match field with
        | Types.Index i -> Index i
        | Types.Name label -> Name (Label.get_name label)
        | Types.Expr _ -> failwith __LOC__
      in
      let field =
        Interpreter.get_field
          ~span
          ~state:interpreter
          ~result_ty:expr.data.signature.ty
          (Interpreter.read_place ~span obj)
          ~obj_mut:true
          member
      in
      (match field with
       | Place (~mut:_, field_place) -> Some field_place
       | RefBlocked _ -> failwith __LOC__)
    | _ -> None

  and generate_claim (ty : ty) : string =
    let c_ty = transpile_ty ty in
    let ctx = Effect.perform GetCtx in
    let ty = mono_ty ty in
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked _value) -> failwith __LOC__
    | _ ->
      let claim_name = ref None in
      let do_prepend = ref false in
      (* Log.info (fun log -> log "Checking in ValueMap: %a" Ty.print ty); *)
      (* let old_captured_types = ctx.captured_types in *)
      let ty_as_value = V_Ty ty |> Value.inferred ~span in
      ctx.claim_fns
      <- ctx.claim_fns
         |> ValueMap.update ty_as_value (fun name ->
           let name =
             match name with
             | Some name -> name
             | None ->
               let name = gen_name (ty_to_string c_ty ^ "_claim") in
               do_prepend := true;
               name
           in
           claim_name := Some name;
           Some name);
      let claim_name = !claim_name |> Option.get in
      if !do_prepend
      then (
        let claim_impl = generate_claim_impl ty in
        add_claim_impl_with_type_erased claim_name (transpile_ty ty) claim_impl);
      claim_name

  and add_claim_impl_with_type_erased
        (claim_name : string)
        (ty : C_ast.ty)
        (claim_impl : C_ast.fn_def)
    =
    let ctx = Effect.perform GetCtx in
    ctx.fns <- ctx.fns |> StringMap.add claim_name claim_impl;
    let claim_impl_type_erased : C_ast.fn_def =
      { comment = None
      ; args =
          [ { name = "place"; ty = T_Ptr T_Void }
          ; { name = "result"; ty = T_Ptr T_Void }
          ]
      ; result_ty = T_Void
      ; body =
          new_block (fun () ->
            insert_stmt
              (S_Assign
                 { assignee =
                     P_Deref
                       (Pure_Cast
                          { value = Pure_Copy (P_Ident "result"); target = T_Ptr ty })
                 ; value =
                     E_Apply
                       { f = Pure_Copy (P_Ident claim_name)
                       ; args =
                           [ Pure_Cast
                               { value = Pure_Copy (P_Ident "place"); target = T_Ptr ty }
                           ]
                       }
                 }))
      }
    in
    ctx.fns
    <- ctx.fns |> StringMap.add (claim_name ^ "_type_erased") claim_impl_type_erased

  and generate_claim_impl (ty : ty) : C_ast.fn_def =
    let ty_ty = ty in
    let arg_name = "place" in
    { comment = Some (make_string "claim for %a" Ty.print ty)
    ; args = [ { name = arg_name; ty = T_Ptr (transpile_ty ty) } ]
    ; result_ty = transpile_ty ty
    ; body =
        new_block (fun () ->
          let ty =
            match ty.var |> Inference.Var.inferred_opt with
            | None -> fail "can't generate_claim_impl for not inferred"
            | Some ty -> ty
          in
          let copy : C_ast.expr =
            E_Pure (Pure_Copy (P_Deref (Pure_Copy (P_Ident arg_name))))
          in
          with_return (fun { return } ->
            let result =
              match ty with
              | Types.T_Unit -> copy
              | Types.T_Bool -> copy
              | Types.T_Int32 -> copy
              | Types.T_UInt32 -> copy
              | Types.T_Int64 -> copy
              | Types.T_UInt64 -> copy
              | Types.T_Float32 -> copy
              | Types.T_Float64 -> copy
              | Types.T_StringView -> copy
              | Types.T_String ->
                E_Apply
                  { f = Pure_Native [ N_Raw "String_claim" ]
                  ; args = [ Pure_Copy (P_Ident arg_name) ]
                  }
              | Types.T_Char -> copy
              | Types.T_Box boxed ->
                E_Apply
                  { f =
                      Pure_Native
                        [ N_Raw "Box_"
                        ; N_Raw (ty_to_string (transpile_ty boxed))
                        ; N_Raw "_claim"
                        ]
                  ; args = [ Pure_Copy (P_Ident arg_name) ]
                  }
              | Types.T_Ref _ -> copy
              | Types.T_Variant variant ->
                (match variant.variants |> Row.await_inferred_to_list with
                 | [] -> copy
                 | variants ->
                   insert_stmt
                     (S_Switch
                        { value =
                            E_Pure
                              (Pure_Copy
                                 (P_Field
                                    { obj = P_Deref (Pure_Copy (P_Ident arg_name))
                                    ; field = "tag"
                                    }))
                        ; cases =
                            variants
                            |> List.map
                                 (fun
                                     ((label, data) : Label.t * Types.ty_variant_data)
                                      : C_ast.switch_case
                                    ->
                                    let tag =
                                      C_ast.Pure_Copy
                                        (P_Ident (variant_tag_name ty_ty label))
                                    in
                                    { value = tag
                                    ; body =
                                        new_block (fun () ->
                                          let fields : (string * C_ast.expr) list =
                                            [ "tag", E_Pure tag ]
                                          in
                                          let fields =
                                            match data.data with
                                            | None -> fields
                                            | Some data ->
                                              let data_value =
                                                claim_c
                                                  (P_Field
                                                     { obj =
                                                         P_Field
                                                           { obj =
                                                               P_Deref
                                                                 (Pure_Copy
                                                                    (P_Ident arg_name))
                                                           ; field = "data"
                                                           }
                                                     ; field =
                                                         make_correct_ident
                                                           (Label.get_name label)
                                                     })
                                                  data
                                              in
                                              fields
                                              @ [ ( "data." ^ Label.get_name label
                                                  , data_value )
                                                ]
                                          in
                                          insert_stmt
                                            (S_Return (compound_literal ty_ty fields)))
                                    })
                        ; default = None
                        });
                   return ())
              | Types.T_Tuple tuple ->
                compound_literal
                  ty_ty
                  (tuple.tuple
                   |> Tuple.to_seq
                   |> List.of_seq
                   |> List.map
                        (fun
                            ((member, field) : Tuple.member * Types.ty_tuple_field)
                             : (string * C_ast.expr)
                           ->
                           ( member_name member
                           , E_Apply
                               { f = Pure_Copy (P_Ident (generate_claim field.ty))
                               ; args =
                                   [ Pure_AddrOf
                                       (P_Field
                                          { obj = P_Deref (Pure_Copy (P_Ident arg_name))
                                          ; field = member_name member
                                          })
                                   ]
                               } )))
              | Types.T_List _ ->
                E_Apply
                  { f =
                      Pure_Native
                        [ N_Raw (ty_to_string (transpile_ty ty_ty)); N_Raw "_claim" ]
                  ; args = [ Pure_Copy (P_Ident arg_name) ]
                  }
              | Types.T_Ty -> copy
              | Types.T_Fn { is_closure; _ } ->
                if is_closure |> Inference.await_inferred_simple
                then
                  E_Apply
                    { f =
                        Pure_Copy (P_Ident (ty_to_string (transpile_ty ty_ty) ^ "_claim"))
                    ; args = [ Pure_Copy (P_Ident arg_name) ]
                    }
                else copy
              | Types.T_Generic _ -> copy
              | Types.T_Ast -> copy
              | Types.T_UnwindToken _ -> copy
              | Types.T_Target -> copy
              | Types.T_ContextTy -> copy
              | Types.T_ImplicitContext -> copy
              | Types.T_CompilerScope -> copy
              | Types.T_Opaque _ -> copy
              | Types.T_Blocked _ -> copy
              | Types.T_Error -> copy
            in
            insert_stmt (S_Return result)))
    }

  and generate_dbg_write (ty : ty) : string =
    let ctx = Effect.perform GetCtx in
    let c_ty = transpile_ty ty in
    let ty = mono_ty ty in
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked _value) -> failwith __LOC__
    | _ ->
      let dbg_write_name = ref None in
      let do_prepend = ref false in
      (* Log.info (fun log -> log "Checking in ValueMap: %a" Ty.print ty); *)
      (* let old_captured_types = ctx.captured_types in *)
      let ty_as_value = V_Ty ty |> Value.inferred ~span in
      ctx.dbg_write_fns
      <- ctx.dbg_write_fns
         |> ValueMap.update ty_as_value (fun name ->
           let name =
             match name with
             | Some name -> name
             | None ->
               let name = gen_name (ty_to_string c_ty ^ "_dbg_write") in
               do_prepend := true;
               name
           in
           dbg_write_name := Some name;
           Some name);
      let dbg_write_name = !dbg_write_name |> Option.get in
      if !do_prepend
      then (
        let dbg_write_impl = generate_dbg_write_impl ty in
        add_dbg_write_impl_with_type_erased
          dbg_write_name
          (transpile_ty ty)
          dbg_write_impl);
      dbg_write_name

  and add_dbg_write_impl_with_type_erased
        (dbg_write_name : string)
        (ty : C_ast.ty)
        (dbg_write_impl : C_ast.fn_def)
    =
    let ctx = Effect.perform GetCtx in
    ctx.fns <- ctx.fns |> StringMap.add dbg_write_name dbg_write_impl;
    let dbg_write_impl_type_erased : C_ast.fn_def =
      { comment = None
      ; args =
          [ { name = "value"; ty = T_Ptr T_Void }
          ; { name = "fmt"; ty = T_Ptr (T_Named "Kast_Formatter") }
          ]
      ; result_ty = T_Void
      ; body =
          new_block (fun () ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Copy (P_Ident dbg_write_name)
                    ; args =
                        [ Pure_Cast
                            { value = Pure_Copy (P_Ident "value"); target = T_Ptr ty }
                        ; Pure_Copy (P_Ident "fmt")
                        ]
                    })))
      }
    in
    ctx.fns
    <- ctx.fns
       |> StringMap.add (dbg_write_name ^ "_type_erased") dbg_write_impl_type_erased

  and generate_dbg_write_impl (ty : ty) : C_ast.fn_def =
    let ty_ty = ty in
    let var = "value" in
    let fmt_var = "fmt" in
    { comment = Some (make_string "dbg_write for %a" Ty.print ty)
    ; args =
        [ { name = var; ty = T_Ptr (transpile_ty ty) }
        ; { name = fmt_var; ty = T_Ptr (T_Named "Kast_Formatter") }
        ]
    ; result_ty = T_Void
    ; body =
        new_block (fun () ->
          let ty =
            match ty.var |> Inference.Var.inferred_opt with
            | None -> fail "can't generate_dbg_write_impl for not inferred"
            | Some ty -> ty
          in
          let printf (f : string) (args : C_ast.pure_expr list) =
            insert_stmt
              (S_Native
                 ([ C_ast.N_Raw "Kast_Formatter_printf(fmt, "
                  ; N_Raw (make_string "%a" String.print_debug f)
                  ]
                  @ (args
                     |> List.map (fun arg : C_ast.native_expr_part list ->
                       [ N_Raw ", "; N_Interpolated arg ])
                     |> List.flatten)
                  @ [ N_Raw ")" ]))
          in
          let println () =
            insert_stmt (S_Native [ N_Raw "Kast_Formatter_println(fmt)" ])
          in
          let inc_indent () =
            insert_stmt (S_Native [ N_Raw "Kast_Formatter_inc_indent(fmt)" ])
          in
          let dec_indent () =
            insert_stmt (S_Native [ N_Raw "Kast_Formatter_dec_indent(fmt)" ])
          in
          let printf_primitive f =
            printf f [ Pure_Copy (P_Deref (Pure_Copy (P_Ident var))) ]
          in
          match ty with
          | Types.T_Unit -> printf "()" []
          | Types.T_Bool ->
            insert_stmt
              (S_If
                 { cond = E_Pure (Pure_Copy (P_Ident var))
                 ; then_case = new_block (fun () -> printf "true" [])
                 ; else_case = Some (new_block (fun () -> printf "false" []))
                 })
          | Types.T_Int32 -> printf_primitive "%d"
          | Types.T_UInt32 -> printf_primitive "%u"
          | Types.T_Int64 -> printf_primitive "%lld"
          | Types.T_UInt64 -> printf_primitive "%llu"
          | Types.T_Float32 -> printf_primitive "%g"
          | Types.T_Float64 -> printf_primitive "%g"
          | Types.T_StringView ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Native [ N_Raw "StringView_dbg_write" ]
                    ; args = [ Pure_Copy (P_Ident var); Pure_Copy (P_Ident fmt_var) ]
                    }))
          | Types.T_String ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Native [ N_Raw "String_dbg_write" ]
                    ; args = [ Pure_Copy (P_Ident var); Pure_Copy (P_Ident fmt_var) ]
                    }))
          | Types.T_Char ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Native [ N_Raw "Char_dbg_write" ]
                    ; args = [ Pure_Copy (P_Ident var); Pure_Copy (P_Ident fmt_var) ]
                    }))
          | Types.T_Box boxed ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f =
                        Pure_Native
                          [ N_Raw "Box_"
                          ; N_Raw (ty_to_string (transpile_ty boxed))
                          ; N_Raw "_dbg_write"
                          ]
                    ; args = [ Pure_Copy (P_Ident var); Pure_Copy (P_Ident fmt_var) ]
                    }))
          | Types.T_Ref { mut; referenced } ->
            printf (if IsMutable.await_inferred mut then "&mut " else "&") [];
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Copy (P_Ident (generate_dbg_write referenced))
                    ; args =
                        [ Pure_Copy (P_Deref (Pure_Copy (P_Ident var)))
                        ; Pure_Copy (P_Ident fmt_var)
                        ]
                    }))
          | Types.T_Variant variant ->
            (match variant.variants |> Row.await_inferred_to_list with
             | [] -> ()
             | variants ->
               insert_stmt
                 (S_Switch
                    { value =
                        E_Pure
                          (Pure_Copy
                             (P_Field
                                { obj = P_Deref (Pure_Copy (P_Ident var)); field = "tag" }))
                    ; cases =
                        variants
                        |> List.map
                             (fun
                                 ((label, data) : Label.t * Types.ty_variant_data)
                                  : C_ast.switch_case
                                ->
                                { value =
                                    Pure_Copy (P_Ident (variant_tag_name ty_ty label))
                                ; body =
                                    new_block (fun () ->
                                      printf (make_string ":%a" Label.print label) [];
                                      match data.data with
                                      | None -> ()
                                      | Some data ->
                                        printf " " [];
                                        insert_stmt
                                          (S_Expr
                                             (E_Apply
                                                { f =
                                                    Pure_Copy
                                                      (P_Ident (generate_dbg_write data))
                                                ; args =
                                                    [ Pure_AddrOf
                                                        (P_Field
                                                           { obj =
                                                               P_Field
                                                                 { obj =
                                                                     P_Deref
                                                                       (Pure_Copy
                                                                          (P_Ident var))
                                                                 ; field = "data"
                                                                 }
                                                           ; field =
                                                               make_correct_ident
                                                                 (Label.get_name label)
                                                           })
                                                    ; Pure_Copy (P_Ident "fmt")
                                                    ]
                                                })))
                                })
                    ; default = None
                    }))
          | Types.T_Tuple tuple ->
            printf "{" [];
            inc_indent ();
            println ();
            tuple.tuple
            |> Tuple.iter (fun member (field : Types.ty_tuple_field) ->
              (match member with
               | Index _ -> ()
               | Name name -> printf (make_string ".%s = " name) []);
              insert_stmt
                (S_Expr
                   (E_Apply
                      { f = Pure_Copy (P_Ident (generate_dbg_write field.ty))
                      ; args =
                          [ Pure_AddrOf
                              (P_Field
                                 { obj = P_Deref (Pure_Copy (P_Ident var))
                                 ; field = member_name member
                                 })
                          ; Pure_Copy (P_Ident fmt_var)
                          ]
                      }));
              printf "," [];
              println ());
            dec_indent ();
            printf "}" []
          | Types.T_List _ ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f =
                        Pure_Copy
                          (P_Ident (ty_to_string (transpile_ty ty_ty) ^ "_dbg_write"))
                    ; args = [ Pure_Copy (P_Ident var); Pure_Copy (P_Ident fmt_var) ]
                    }))
          | Types.T_Ty -> printf "<ty>" []
          | Types.T_Fn { is_closure; _ } ->
            if is_closure |> Inference.await_inferred_simple
            then printf "<closure>" []
            else printf "<fn>" []
          | Types.T_Generic _ -> printf "<generic>" []
          | Types.T_Ast -> printf "<ast>" []
          | Types.T_UnwindToken _ -> printf "<unwind token>" []
          | Types.T_Target -> printf "<target>" []
          | Types.T_ContextTy -> printf "<context ty>" []
          | Types.T_ImplicitContext -> printf "<implicit context>" []
          | Types.T_CompilerScope -> printf "<compiler scope>" []
          | Types.T_Opaque _ -> printf "<opaque>" []
          | Types.T_Blocked _ -> printf "<blocked>" []
          | Types.T_Error -> printf "<error>" [])
    }

  and generate_drop (ty : ty) : string =
    let ctx = Effect.perform GetCtx in
    let c_ty = transpile_ty ty in
    let ty = mono_ty ty in
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked _value) -> failwith __LOC__
    | _ ->
      let drop_name = ref None in
      let do_prepend = ref false in
      (* Log.info (fun log -> log "Checking in ValueMap: %a" Ty.print ty); *)
      (* let old_captured_types = ctx.captured_types in *)
      let ty_as_value = V_Ty ty |> Value.inferred ~span in
      ctx.drop_fns
      <- ctx.drop_fns
         |> ValueMap.update ty_as_value (fun name ->
           let name =
             match name with
             | Some name -> name
             | None ->
               let name = gen_name (ty_to_string c_ty ^ "_drop") in
               do_prepend := true;
               name
           in
           drop_name := Some name;
           Some name);
      let drop_name = !drop_name |> Option.get in
      if !do_prepend
      then (
        let drop_impl = generate_drop_impl ty in
        add_drop_impl_with_type_erased drop_name (transpile_ty ty) drop_impl);
      drop_name

  and add_drop_impl_with_type_erased
        (drop_name : string)
        (ty : C_ast.ty)
        (drop_impl : C_ast.fn_def)
    =
    let ctx = Effect.perform GetCtx in
    ctx.fns <- ctx.fns |> StringMap.add drop_name drop_impl;
    let drop_impl_type_erased : C_ast.fn_def =
      { comment = None
      ; args = [ { name = "value"; ty = T_Ptr T_Void } ]
      ; result_ty = T_Void
      ; body =
          new_block (fun () ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Copy (P_Ident drop_name)
                    ; args =
                        [ Pure_Copy
                            (P_Deref
                               (Pure_Cast
                                  { value = Pure_Copy (P_Ident "value")
                                  ; target = T_Ptr ty
                                  }))
                        ]
                    })))
      }
    in
    ctx.fns <- ctx.fns |> StringMap.add (drop_name ^ "_type_erased") drop_impl_type_erased

  and generate_drop_impl (ty : ty) : C_ast.fn_def =
    let ty_ty = ty in
    let var = "value" in
    { comment = Some (make_string "Drop for %a" Ty.print ty)
    ; args = [ { name = var; ty = transpile_ty ty } ]
    ; result_ty = T_Void
    ; body =
        new_block (fun () ->
          let todo = () in
          let ty =
            match ty.var |> Inference.Var.inferred_opt with
            | None -> fail "can't generate_drop_impl for not inferred"
            | Some ty -> ty
          in
          match ty with
          | Types.T_Unit -> ()
          | Types.T_Bool -> ()
          | Types.T_Int32 -> ()
          | Types.T_UInt32 -> ()
          | Types.T_Int64 -> ()
          | Types.T_UInt64 -> ()
          | Types.T_Float32 -> ()
          | Types.T_Float64 -> ()
          | Types.T_StringView -> ()
          | Types.T_String ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f = Pure_Native [ N_Raw "String_drop" ]
                    ; args = [ Pure_Copy (P_Ident var) ]
                    }))
          | Types.T_Char -> ()
          | Types.T_Box boxed ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f =
                        Pure_Native
                          [ N_Raw "Box_"
                          ; N_Raw (ty_to_string (transpile_ty boxed))
                          ; N_Raw "_drop"
                          ]
                    ; args = [ Pure_Copy (P_Ident var) ]
                    }))
          | Types.T_Ref _ -> ()
          | Types.T_Variant variant ->
            (match variant.variants |> Row.await_inferred_to_list with
             | [] -> ()
             | variants ->
               insert_stmt
                 (S_Switch
                    { value =
                        E_Pure (Pure_Copy (P_Field { obj = P_Ident var; field = "tag" }))
                    ; cases =
                        variants
                        |> List.map
                             (fun
                                 ((label, data) : Label.t * Types.ty_variant_data)
                                  : C_ast.switch_case
                                ->
                                { value =
                                    Pure_Copy (P_Ident (variant_tag_name ty_ty label))
                                ; body =
                                    new_block (fun () ->
                                      match data.data with
                                      | None -> ()
                                      | Some data ->
                                        insert_stmt
                                          (S_Expr
                                             (E_Apply
                                                { f =
                                                    Pure_Copy
                                                      (P_Ident (generate_drop data))
                                                ; args =
                                                    [ Pure_Copy
                                                        (P_Field
                                                           { obj =
                                                               P_Field
                                                                 { obj = P_Ident var
                                                                 ; field = "data"
                                                                 }
                                                           ; field =
                                                               make_correct_ident
                                                                 (Label.get_name label)
                                                           })
                                                    ]
                                                })))
                                })
                    ; default = None
                    }))
          | Types.T_Tuple tuple ->
            tuple.tuple
            |> Tuple.iter (fun member (field : Types.ty_tuple_field) ->
              insert_stmt
                (S_Expr
                   (E_Apply
                      { f = Pure_Copy (P_Ident (generate_drop field.ty))
                      ; args =
                          [ Pure_Copy
                              (P_Field { obj = P_Ident var; field = member_name member })
                          ]
                      })))
          | Types.T_List _ ->
            insert_stmt
              (S_Expr
                 (E_Apply
                    { f =
                        Pure_Copy (P_Ident (ty_to_string (transpile_ty ty_ty) ^ "_drop"))
                    ; args = [ Pure_Copy (P_Ident var) ]
                    }))
          | Types.T_Ty -> ()
          | Types.T_Fn { is_closure; _ } ->
            let is_closure = is_closure |> Inference.await_inferred_simple in
            if is_closure
            then
              insert_stmt
                (S_Expr
                   (E_Apply
                      { f =
                          Pure_Copy
                            (P_Ident (ty_to_string (transpile_ty ty_ty) ^ "_drop"))
                      ; args = [ Pure_Copy (P_Ident var) ]
                      }))
          | Types.T_Generic _ -> ()
          | Types.T_Ast -> ()
          | Types.T_UnwindToken _ -> ()
          | Types.T_Target -> ()
          | Types.T_ContextTy -> ()
          | Types.T_ImplicitContext -> todo
          | Types.T_CompilerScope -> ()
          | Types.T_Opaque _ -> ()
          | Types.T_Blocked _ -> ()
          | Types.T_Error -> ())
    }

  and transpile_ty (ty : ty) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let ty = mono_ty ty in
    match ty.var |> Inference.Var.inferred_opt with
    | None -> fail "transpiling not inferred type %a" Ty.print ty
    | Some (T_Blocked value) -> T_Ptr T_Void
    | Some ty_shape ->
      let ty_as_value = V_Ty ty |> Value.inferred ~span in
      let prepend = ref None in
      let ty_name : C_ast.ty =
        match ctx.captured_types |> ValueMap.find_opt ty_as_value with
        | Some Inprogress -> fail "recursive type %a" Ty.print ty
        | Some (Completed ty) -> ty
        | None ->
          ctx.captured_types <- ctx.captured_types |> ValueMap.add ty_as_value Inprogress;
          let ty =
            match transpile_ty_shape ty_shape with
            | Alias t -> t
            | T_Named { name; def } ->
              prepend
              := Some
                   (fun () ->
                     let def = def () in
                     ctx.types <- ctx.types |> StringMap.add name def);
              T_Named name
          in
          ctx.captured_types
          <- ctx.captured_types |> ValueMap.add ty_as_value (Completed ty);
          ty
      in
      (match !prepend with
       | None -> ()
       | Some f -> f ());
      ty_name

  and variant_tag_ty (ty_name : string) (ty : Types.ty_variant) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let name = ty_name ^ "_TAG" in
    let variant_names =
      ty.variants
      |> Row.await_inferred_to_list
      |> List.map (fun (label, _) -> variant_tag_name_impl ty_name label)
    in
    let def : C_ast.ty_def =
      { shape = TD_Enum (StringSet.of_list variant_names)
      ; comment = Some (make_string "Tag for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    T_Named name

  and variant_data_ty_impl (ty_name : string) (ty : Types.ty_variant) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let name = ty_name ^ "_DATA" in
    let variants =
      ty.variants
      |> Row.await_inferred_to_list
      |> List.map (fun ((label, { data }) : Label.t * Types.ty_variant_data) ->
        let name = make_correct_ident (Label.get_name label) in
        let ty =
          match data with
          | Some ty -> transpile_ty ty
          | None -> T_Unit
        in
        name, ty)
    in
    let def : C_ast.ty_def =
      { shape = TD_Union (StringMap.of_list variants)
      ; comment = Some (make_string "Data for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    T_Named name

  and add_include s =
    let ctx = Effect.perform GetCtx in
    ctx.includes <- ctx.includes |> StringSet.add s

  and variant_needs_tag (ty : Types.ty_variant) : bool =
    ty.variants |> Row.await_inferred_to_list |> List.length > 0

  and uninitialized (ty : C_ast.ty) : C_ast.expr =
    match ty with
    | T_Unit -> E_Pure Pure_Unit
    | _ -> E_Pure (Pure_Compound { ty; fields = [] })

  and mono_value (value : value) : value =
    profile "mono_value" (fun () ->
      let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
      Inference.Var.setup_default_if_needed value.var;
      let value =
        Interpreter.monomorphized_value
          ~span
          ~state:interpreter
          (Inference.Var.recurse_id value.var)
          value
      in
      Inference.Var.setup_default_if_needed value.var;
      value)

  and mono_ty (ty : ty) : ty =
    profile "mono_ty" (fun () ->
      let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
      Inference.Var.setup_default_if_needed ty.var;
      let ty =
        Interpreter.monomorphized_ty
          ~span
          ~state:interpreter
          (Inference.Var.recurse_id ty.var)
          ty
      in
      Inference.Var.setup_default_if_needed ty.var;
      ty)

  and ty_repr (ty : ty) : ty = ty |> mono_ty |> Ty.await_inferred |> ty_shape_repr

  and ty_shape_repr (ty : Types.ty_shape) : ty =
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    match ty with
    | T_UnwindToken { result } ->
      let impl =
        interpreter.natives.by_name
        |> StringMap.find "backend.c.UnwindToken"
        <| Ty.new_not_inferred ~scope:(Some interpreter.scope) ~span
      in
      let impl =
        Interpreter.instantiate
          span
          interpreter
          impl
          (Interpreter.Natives.make_single_arg_infer
             ~span
             (Value.inferred ~span (V_Ty result)))
        |> Value.expect_ty
        |> Option.unwrap_or_else (fun () -> fail "UnwindToken not a type???")
      in
      impl
    | _ -> fail "no repr for %a" Ty.Shape.print ty

  and transpile_ty_shape (ty : Types.ty_shape) : transpiled_ty_shape =
    let ctx = Effect.perform GetCtx in
    let make_with (shape : unit -> C_ast.ty_def_shape) () : C_ast.ty_def =
      { shape = shape (); comment = Some (make_string "%a" Print.print_ty_shape ty) }
    in
    let alias (c : C_ast.ty) : transpiled_ty_shape = Alias c in
    let runtime_defined (name : string) : transpiled_ty_shape = alias (T_Named name) in
    match ty with
    | Types.T_Unit -> alias T_Unit
    | Types.T_Bool -> runtime_defined "Bool"
    | Types.T_Int32 -> runtime_defined "Int32"
    | Types.T_UInt32 -> runtime_defined "UInt32"
    | Types.T_Int64 -> runtime_defined "Int64"
    | Types.T_UInt64 -> runtime_defined "UInt64"
    | Types.T_Float32 -> runtime_defined "Float32"
    | Types.T_Float64 -> runtime_defined "Float64"
    | Types.T_String -> runtime_defined "String"
    | Types.T_StringView -> runtime_defined "StringView"
    | Types.T_Char -> runtime_defined "Char"
    | Types.T_Box boxed ->
      let boxed = transpile_ty boxed in
      let macro_arg = ty_to_string boxed in
      (match ctx.runtime_defined_box_types |> StringMap.find_opt macro_arg with
       | Some name -> runtime_defined name
       | None ->
         T_Named
           { name = "Box_" ^ macro_arg
           ; def =
               make_with (fun () : C_ast.ty_def_shape ->
                 TD_Raw
                   { def = make_string "define_Box(%s)" macro_arg
                   ; impl = Some (make_string "impl_Box(%s)" macro_arg)
                   ; need_declared = [ T_Named macro_arg ]
                   ; need_completed = []
                   })
           })
    | Types.T_Ref { mut = _; referenced } -> alias (T_Ptr (transpile_ty referenced))
    | Types.T_Variant ty ->
      let ty_name =
        match ty.name |> OptionalName.await_inferred with
        | Some name -> gen_name (make_string "%a" Print.print_name_shape name)
        | None -> gen_name "anonymous_variant"
      in
      T_Named
        { name = ty_name
        ; def =
            make_with (fun () ->
              if variant_needs_tag ty
              then
                TD_Struct
                  (StringMap.of_list
                     [ "tag", variant_tag_ty ty_name ty
                     ; "data", variant_data_ty_impl ty_name ty
                     ])
              else
                TD_Struct (StringMap.singleton "data" (variant_data_ty_impl ty_name ty)))
        }
    | Types.T_Tuple { name = ty_name; tuple } ->
      let ty_name =
        match ty_name |> OptionalName.await_inferred with
        | Some name -> gen_name (make_string "%a" Print.print_name_shape name)
        | None -> gen_name "anonymous_tuple"
      in
      let fields () =
        tuple
        |> Tuple.to_seq
        |> Seq.map
             (fun
                 ((member, field) : Tuple.member * Types.ty_tuple_field)
                  : (string * C_ast.ty)
                -> member_name member, transpile_ty field.ty)
        |> StringMap.of_seq
      in
      if !boxed_structs
      then (
        let def =
          ({ shape = C_ast.TD_Struct (fields ())
           ; comment = Some (make_string "%a" Print.print_ty_shape ty)
           }
           : C_ast.ty_def)
        in
        ctx.types <- ctx.types |> StringMap.add ty_name def;
        alias (T_Ptr (T_Named ty_name)))
      else T_Named { name = ty_name; def = make_with (fun () -> TD_Struct (fields ())) }
    | Types.T_List { element_ty } ->
      let element_ty = transpile_ty element_ty in
      let macro_arg = ty_to_string element_ty in
      (match ctx.runtime_defined_list_types |> StringMap.find_opt macro_arg with
       | Some name -> runtime_defined name
       | None ->
         T_Named
           { name = "ArrayList_" ^ macro_arg
           ; def =
               make_with (fun () ->
                 TD_Raw
                   { def = make_string "define_ArrayList(%s)" macro_arg
                   ; impl = Some (make_string "impl_ArrayList(%s)" macro_arg)
                   ; need_declared = [ T_Named macro_arg ]
                   ; need_completed = []
                   })
           })
    | Types.T_Ty -> alias (T_Ptr (T_Named "TypeInfo"))
    | Types.T_Fn { is_closure; call_convention; args; result } ->
      let is_closure = is_closure |> Inference.await_inferred_simple in
      let call_convention = call_convention |> Inference.await_inferred_simple in
      let args = args.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap in
      let args =
        args.tuple
        |> Tuple.to_seq
        |> Seq.map (fun ((_member, field) : Tuple.member * Types.ty_tuple_field) ->
          transpile_ty field.ty)
        |> List.of_seq
      in
      let result_ty = transpile_ty result in
      let result_ty : C_ast.ty =
        match result_ty with
        | T_Unit -> T_Void
        | other -> other
      in
      let define_macro_args = ty_to_string result_ty :: (args |> List.map ty_to_string) in
      let args = if is_closure then [ C_ast.T_Ptr T_Void ] @ args else args in
      let args =
        match call_convention with
        | None -> [ C_ast.T_Ptr (T_Raw { c = "Context"; is_primitive = false }) ] @ args
        | Some "C" -> args
        | _ -> fail "unknown call convention"
      in
      (match is_closure with
       | true ->
         Log.trace (fun log ->
           log "Looking for %a" (List.print String.print) define_macro_args);
         (match
            ctx.runtime_defined_closure_types |> StringListMap.find_opt define_macro_args
          with
          | Some name -> runtime_defined name
          | None ->
            let name = List.fold_left (fun s n -> s ^ "_" ^ n) "Fn" define_macro_args in
            let macro_arg =
              List.fold_left (fun s n -> s ^ ", " ^ n) name define_macro_args
            in
            T_Named
              { name
              ; def =
                  make_with (fun () ->
                    TD_Raw
                      { def = make_string "define_closure_type(%s)" macro_arg
                      ; impl = None
                      ; need_declared =
                          List.map (fun name : C_ast.ty -> T_Named name) define_macro_args
                      ; need_completed = []
                      })
              })
       | false ->
         let name = List.fold_left (fun s n -> s ^ "_" ^ n) "N_RawFn" define_macro_args in
         T_Named { name; def = make_with (fun () -> TD_Fn { args; result_ty }) })
    | Types.T_Generic _ when true -> alias T_Unit
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
      let name = gen_name "generic" in
      T_Named { name; def = make_with (fun () -> TD_Fn { args; result_ty }) }
    | Types.T_Ast -> alias T_Unit
    | Types.T_UnwindToken _ -> alias (T_Ptr (transpile_ty (ty_shape_repr ty)))
    | Types.T_Target -> failwith __LOC__
    | Types.T_ContextTy ->
      (* TODO maybe? *)
      alias T_Unit
    | Types.T_ImplicitContext -> runtime_defined "Context"
    | Types.T_CompilerScope -> alias T_Unit
    | Types.T_Opaque { name; native_name } ->
      let name = gen_name (make_string "%a" Print.print_name name) in
      (match native_name with
       | Some native_name ->
         T_Named
           { name
           ; def =
               make_with (fun () ->
                 TD_Alias (T_Raw { c = native_name; is_primitive = false }))
           }
       | None ->
         fail
           "native name must be set for opaque types when transpiling to C: %a"
           Ty.Shape.print
           ty)
    | Types.T_Blocked _ -> failwith __LOC__
    | Types.T_Error -> fail "transpiling error ty"

  and does_match (pattern : pattern) (pure_place_expr : C_ast.place_expr)
    : C_ast.pure_expr
    =
    match pattern.shape with
    | Types.P_Placeholder -> Pure_Literal (L_Bool true)
    | Types.P_Ref { mut = _; referenced } ->
      does_match referenced (P_Deref (Pure_Copy pure_place_expr))
    | Types.P_Unit -> Pure_Literal (L_Bool true)
    | Types.P_Binding _ -> Pure_Literal (L_Bool true)
    | Types.P_Tuple { parts; _ } ->
      let unnamed_idx = ref 0 in
      let had_unpack = ref false in
      parts
      |> List.map (fun (part : pattern Types.tuple_part_of) : C_ast.pure_expr ->
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
          does_match
            field
            (P_Field
               { obj =
                   (if !boxed_structs
                    then P_Deref (Pure_Copy pure_place_expr)
                    else pure_place_expr)
               ; field = member_name member
               })
        | Unpack pattern ->
          (match pattern.shape with
           | P_Placeholder -> ()
           | P_Binding _ -> ()
           | _ -> failwith __LOC__);
          had_unpack := true;
          Pure_Literal (L_Bool true))
      |> List.fold_left
           (fun a b : C_ast.pure_expr -> Pure_And (a, b))
           (Pure_Literal (L_Bool true))
    | Types.P_Variant { label_span = _; label; value } ->
      let tag_matches : C_ast.pure_expr =
        Pure_Equal
          ( Pure_Copy (P_Field { obj = pure_place_expr; field = "tag" })
          , Pure_Copy (P_Ident (variant_tag_name pattern.data.signature.ty label)) )
      in
      (match value with
       | Some value ->
         Pure_And
           ( tag_matches
           , does_match
               value
               (P_Field
                  { obj = P_Field { obj = pure_place_expr; field = "data" }
                  ; field = make_correct_ident (Label.get_name label)
                  }) )
       | None -> tag_matches)
    | Types.P_Error -> failwith __LOC__

  and pattern_match (pattern : pattern) (pure_place_expr : C_ast.place_expr) : unit =
    match pattern.shape with
    | Types.P_Placeholder -> ()
    | Types.P_Ref { mut = _; referenced } ->
      pattern_match referenced (P_Deref (Pure_Copy pure_place_expr))
    | Types.P_Unit -> ()
    | Types.P_Binding { bind_mode; binding } ->
      let value : C_ast.expr =
        match bind_mode with
        | Types.Claim -> claim_c pure_place_expr binding.ty
        | Types.ByRef { mut = _ } -> E_Pure (Pure_AddrOf pure_place_expr)
      in
      let_binding binding value
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
            (P_Field
               { obj =
                   (if !boxed_structs
                    then P_Deref (Pure_Copy pure_place_expr)
                    else pure_place_expr)
               ; field = member_name member
               })
        | Unpack pattern ->
          (match pattern.shape with
           | P_Placeholder -> ()
           | _ -> failwith __LOC__);
          had_unpack := true)
    | Types.P_Variant { label; value = data; _ } ->
      (match data with
       | Some data_pattern ->
         pattern_match
           data_pattern
           (P_Field
              { obj = P_Field { obj = pure_place_expr; field = "data" }
              ; field = make_correct_ident (Label.get_name label)
              })
       | None -> ())
    | Types.P_Error -> failwith __LOC__

  and defer (f : unit -> unit) =
    let unwind_ctx = Effect.perform GetUnwindCtx in
    let old_insert_unwind = unwind_ctx.insert_unwind in
    unwind_ctx.insert_unwind
    <- (fun () ->
         f ();
         old_insert_unwind ());
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
    let current_captured = Effect.perform CurrentFnCaptured in
    let captured_interpreter_scope : Interpreter.Scope.t =
      captured |> Option.unwrap_or_else (fun () -> current_captured.interpreter_scope)
    in
    let captured_interpreter =
      { current_captured.interpreter_state with
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
      !(def.captures)
      |> Id.Map.to_list
      |> List.filter_map (fun ((_id, binding) : id * binding) ->
        match captured_interpreter_scope |> Interpreter.Scope.find_opt binding.name with
        | None -> Some binding
        | Some _ -> None)
    in
    let captured_ty_name, captured_binding_places =
      match captured_bindings with
      | [] -> None, Id.Map.empty
      | captures ->
        let name = gen_name "captured" in
        let captured_ty_def : C_ast.ty_def =
          { shape =
              TD_Struct
                (captures
                 |> List.map (fun (binding : binding) ->
                   ( binding_name binding
                   , let ty = transpile_ty binding.ty in
                     if def.is_move then ty else C_ast.T_Ptr ty ))
                 |> StringMap.of_list)
          ; comment = Some (make_string "captured of closure at %a" print_span def_span)
          }
        in
        ctx.types <- ctx.types |> StringMap.add name captured_ty_def;
        let captured_ty_type_info : C_ast.static =
          { name = name ^ "_TypeInfo"; ty = T_Named "TypeInfo"; comment = None }
        in
        let type_info_expr =
          construct_type_info_for_struct
            name
            (captures
             |> List.map (fun (binding : binding) ->
               ( binding_name binding
               , if def.is_move
                 then binding.ty
                 else
                   Ty.inferred
                     ~span
                     (T_Ref
                        { mut = IsMutable.new_inferred ~span true
                        ; referenced = binding.ty
                        }) ))
             |> StringMap.of_list)
        in
        Dynarray.add_last ctx.raw_type_infos (fun () ->
          insert_stmt
            (S_Assign
               { assignee = P_Ident captured_ty_type_info.name
               ; value = type_info_expr ()
               }));
        Dynarray.add_last ctx.statics captured_ty_type_info;
        ( Some name
        , captures
          |> List.map (fun (binding : binding) : (Id.t * C_ast.place_expr) ->
            ( binding.id
            , let field : C_ast.place_expr =
                P_Field
                  { obj = P_Deref (Pure_Copy (P_Ident typed_captured_arg_name))
                  ; field = binding_name binding
                  }
              in
              if def.is_move then field else P_Deref (Pure_Copy field) ))
          |> Id.Map.of_list )
    in
    let captured : current_captured =
      { interpreter_scope = captured_interpreter_scope
      ; bindings = captured_binding_places
      ; interpreter_state = captured_interpreter
      }
    in
    try
      let result_ty = transpile_ty def.body.data.signature.ty in
      let result_ty : C_ast.ty =
        match result_ty with
        | T_Unit -> T_Void
        | other -> other
      in
      let unwind_ctx : unwind_ctx =
        { insert_unwind =
            (fun () ->
              match result_ty with
              | T_Void -> insert_stmt S_ReturnVoid
              | _ -> insert_stmt (S_Return (uninitialized result_ty)))
        ; cleanup_scope_without_unwind = (fun () -> ())
        }
      in
      try
        let args_tuple_ty =
          match def.args.pattern.data.signature.ty |> Ty.await_inferred with
          | T_Tuple { name = _; tuple } -> tuple
          | _ -> fail "fn args are not tuple"
        in
        let args =
          args_tuple_ty
          |> Tuple.to_seq
          |> Seq.map
               (fun
                   ((member, field) : Tuple.member * Types.ty_tuple_field)
                    : C_ast.fn_arg
                  -> { name = member_name member; ty = transpile_ty field.ty })
          |> List.of_seq
        in
        let args =
          if is_closure
          then [ ({ name = captured_arg_name; ty = T_Ptr T_Void } : C_ast.fn_arg) ] @ args
          else args
        in
        let ctx_var = gen_name "ctx" in
        let args =
          match call_convention with
          | None ->
            [ ({ name = ctx_var; ty = T_Ptr (T_Named "Context") } : C_ast.fn_arg) ] @ args
          | Some "C" -> args
          | Some other -> fail "unknown call convension %S" other
        in
        let body : C_ast.block =
          let result_var : C_ast.pure_expr option ref = ref None in
          let block =
            new_block (fun () ->
              let scope : scope = { ctx_place = P_Deref (Pure_Copy (P_Ident ctx_var)) } in
              args_tuple_ty
              |> Tuple.iter (fun member (field : Types.ty_tuple_field) ->
                defer (fun () ->
                  insert_stmt
                    (S_Expr
                       (E_Apply
                          { f = Pure_Copy (P_Ident (generate_drop field.ty))
                          ; args = [ Pure_Copy (P_Ident (member_name member)) ]
                          }))));
              try
                captured_bindings
                |> List.iter (fun (binding : binding) ->
                  insert_stmt
                    (S_Comment (make_string "captured %a" Binding.print binding)));
                (match captured_ty_name with
                 | Some captured_ty_name ->
                   let_c_var
                     (T_Ptr (T_Named captured_ty_name))
                     typed_captured_arg_name
                     (Some (E_Pure (Pure_Copy (P_Ident captured_arg_name))))
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
                    pattern_match field (P_Ident (member_name member))
                  | Unpack packed ->
                    let packed_ty =
                      packed.data.signature.ty
                      |> Ty.await_inferred
                      |> Ty.Shape.expect_tuple
                      |> Option.unwrap
                    in
                    let var = gen_name "packed" in
                    let var_ty = transpile_ty packed.data.signature.ty in
                    let_var packed.data.signature.ty var None;
                    if !boxed_structs
                    then
                      malloc_typed_ptr ~boxed:true packed.data.signature.ty (P_Ident var);
                    packed_ty.tuple
                    |> Tuple.iter (fun packed_member _field ->
                      let member =
                        match packed_member with
                        | Index _ ->
                          let result = Tuple.Member.Index !unnamed_idx in
                          unnamed_idx := !unnamed_idx + 1;
                          result
                        | Name name -> Name name
                      in
                      insert_stmt
                        (S_Assign
                           { assignee =
                               P_Field
                                 { obj = tuple_place var
                                 ; field = member_name packed_member
                                 }
                           ; value = E_Pure (Pure_Copy (P_Ident (member_name member)))
                           }));
                    pattern_match packed (P_Ident var));
                unwind_ctx.cleanup_scope_without_unwind ();
                match eval_expr def.body with
                | None -> ()
                | Some result ->
                  (match result_ty with
                   | T_Unit | T_Void -> insert_stmt (S_Expr result)
                   | _ -> result_var := Some (make_pure result_ty result "fn_result"))
              with
              | effect GetScope, k -> Effect.continue k scope)
          in
          block
          @ (!result_var
             |> Option.map (fun result_expr -> C_ast.S_Return (E_Pure result_expr))
             |> Option.to_list)
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
        { captured =
            captured_bindings
            |> List.map (fun (binding : binding) -> binding.id, binding)
            |> Id.Map.of_list
        ; is_move = def.is_move
        ; captured_ty_name
        ; name
        ; def
        }
      with
      | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx
    with
    | effect CurrentFnCaptured, k -> Effect.continue k captured

  and construct_type_info_for_struct (struct_name : string) (fields : ty StringMap.t)
    : unit -> C_ast.expr
    =
    let drop_fn_name = struct_name ^ "_drop" in
    let claim_fn_name = struct_name ^ "_claim" in
    let drop_fn_impl : C_ast.fn_def =
      { args = [ { name = "value"; ty = T_Named struct_name } ]
      ; result_ty = T_Void
      ; body =
          new_block (fun () ->
            fields
            |> StringMap.iter (fun field_name field_ty ->
              insert_stmt
                (S_Expr
                   (E_Apply
                      { f = Pure_Copy (P_Ident (generate_drop field_ty))
                      ; args =
                          [ Pure_Copy
                              (P_Field { obj = P_Ident "value"; field = field_name })
                          ]
                      }))))
      ; comment = None
      }
    in
    let claim_fn_impl : C_ast.fn_def =
      { args = [ { name = "place"; ty = T_Ptr (T_Named struct_name) } ]
      ; result_ty = T_Named struct_name
      ; body =
          new_block (fun () ->
            insert_stmt
              (S_Return
                 (c_compound_literal
                    (T_Named struct_name)
                    (fields
                     |> StringMap.mapi (fun field_name field_ty : C_ast.expr ->
                       E_Apply
                         { f = Pure_Copy (P_Ident (generate_claim field_ty))
                         ; args =
                             [ Pure_AddrOf
                                 (P_Field
                                    { obj = P_Deref (Pure_Copy (P_Ident "place"))
                                    ; field = field_name
                                    })
                             ]
                         })
                     |> StringMap.to_list))))
      ; comment = None
      }
    in
    add_drop_impl_with_type_erased drop_fn_name (T_Named struct_name) drop_fn_impl;
    add_claim_impl_with_type_erased claim_fn_name (T_Named struct_name) claim_fn_impl;
    fun () ->
      c_compound_literal
        (T_Raw { c = "TypeInfo"; is_primitive = false })
        (([ "name", C_ast.Pure_Literal (L_String struct_name)
          ; ( "alignment"
            , C_ast.Pure_Native [ N_Raw "alignof("; N_Raw struct_name; N_Raw ")" ] )
          ; "size", Pure_Native [ N_Raw "sizeof("; N_Raw struct_name; N_Raw ")" ]
          ; "stride", Pure_Native [ N_Raw "sizeof("; N_Raw struct_name; N_Raw ")" ]
            (* ; "kind", E_Native { parts = [ N_Raw "TypeInfoKind_raw" ] } *)
          ; "drop", Pure_Copy (P_Ident (drop_fn_name ^ "_type_erased"))
          ; "claim", Pure_Copy (P_Ident (claim_fn_name ^ "_type_erased"))
          ]
          @
          if !allocation_stats
          then
            [ ( "allocation_stats"
              , C_ast.Pure_Native [ N_Raw "Kast_type_allocation_stats_new()" ] )
            ]
          else [])
         |> List.map (fun (name, expr) -> name, C_ast.E_Pure expr))

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
    make_string "%s_%d" name (Id.gen ()).value

  and not_inferred (var : _ Inference.var) : C_ast.expr = failwith __LOC__

  and transpile_place (place : place) : C_ast.place_expr =
    transpile_value (Interpreter.read_place ~span place)

  (* TODO should be pure_expr? *)
  and transpile_value (value : value) : C_ast.place_expr =
    let ctx = Effect.perform GetCtx in
    let value = mono_value value in
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked value) -> failwith __LOC__
    | _ ->
      (try
         let value_name = ref None in
         let do_prepend = ref false in
         ctx.captured_values
         <- ctx.captured_values
            |> ValueMap.update value (fun name ->
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
                  let is_closure = fn_ty.is_closure |> Inference.await_inferred_simple in
                  let call_convention =
                    fn_ty.call_convention |> Inference.await_inferred_simple
                  in
                  let f =
                    transpile_fn
                      ~call_convention
                      ~is_closure
                      ~captured:(Some captured)
                      def
                  in
                  if is_closure && f.captured_ty_name |> Option.is_none
                  then
                    Some
                      (compound_literal
                         (Value.ty_of value)
                         [ "captured", E_Pure (Pure_Native [ N_Raw "NULL" ])
                         ; "f", E_Pure (Pure_Copy (P_Ident f.name))
                         ])
                  else Some (E_Pure (Pure_Copy (P_Ident f.name)))
                | V_Generic _ when true -> Some (E_Pure Pure_Unit)
                | V_Generic { fn = { def; captured; _ }; _ } ->
                  (* TODO memoize generics *)
                  let f =
                    transpile_fn
                      ~call_convention:(Some "C")
                      ~is_closure:false
                      ~captured:(Some captured)
                      def
                  in
                  Some (E_Pure (Pure_Copy (P_Ident f.name)))
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
             insert_stmt (S_Assign { assignee = P_Ident value_name; value = value_expr });
             insert_stmt
               (S_Native
                  [ N_Raw
                      ("#ifdef USE_GC\n"
                       ^ "GC_add_roots(&"
                       ^ value_name
                       ^ ", (&"
                       ^ value_name
                       ^ ") + 1);\n"
                       ^ "#endif\n")
                  ]);
             Dynarray.add_last ctx.statics static);
         P_Ident value_name
       with
       | effect GetCurrentBlock, k -> Effect.continue k ctx.init_statics)

  and transpile_value_shape (shape : Types.value_shape) : C_ast.expr =
    match shape with
    | V_Unit -> E_Pure Pure_Unit
    | V_Bool x -> E_Pure (Pure_Literal (L_Bool x))
    | V_Int32 x | V_UInt32 x -> E_Pure (Pure_Literal (L_Int32 x))
    | V_Int64 x | V_UInt64 x -> E_Pure (Pure_Literal (L_Int64 x))
    | V_Float64 x | V_Float32 x -> E_Pure (Pure_Literal (L_Float64 x))
    | V_Char c -> E_Pure (Pure_Literal (L_Int32 (Int32.of_int (Uchar.to_int c))))
    | V_Box _ -> failwith __LOC__
    | V_Ref _ -> failwith __LOC__
    | V_String s ->
      E_Apply
        { f = Pure_Native [ N_Raw "String_from_C_StringView" ]
        ; args = [ Pure_Literal (L_String s) ]
        }
    | V_StringView s ->
      E_Apply
        { f = Pure_Native [ N_Raw "StringView_from_C_StringView" ]
        ; args = [ Pure_Literal (L_String s) ]
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
                , E_Pure
                    (C_ast.Pure_Copy
                       (transpile_value (field.place |> Interpreter.read_place ~span))) ))
        |> List.of_seq
      in
      compound_literal (Value.Shape.ty_of shape) fields
    | V_List { ty = { element_ty }; elements } ->
      E_Block
        (new_block (fun () ->
           let var = gen_name "list" in
           let kast_ty = Value.Shape.ty_of shape in
           let ty = transpile_ty kast_ty in
           let_var
             kast_ty
             var
             (Some
                (E_Native
                   [ N_Raw (ty_to_string ty)
                   ; N_Raw "_new(&"
                   ; N_Raw (type_info_name_for element_ty)
                   ; N_Raw ")"
                   ]))
             ~drop:false;
           elements
           |> Dynarray.iter (fun element ->
             insert_stmt
               (S_Native
                  [ N_Raw (ty_to_string ty)
                  ; N_Raw "_push_back("
                  ; N_Interpolated (Pure_AddrOf (P_Ident var))
                  ; N_Raw ", "
                  ; N_Interpolated (Pure_Copy (transpile_place element))
                  ; N_Raw ")"
                  ]));
           insert_stmt (S_Expr (E_Pure (Pure_Copy (P_Ident var))))))
    | V_Variant { label; data; ty = _ } ->
      let ty = Value.Shape.ty_of shape in
      let fields : (string * C_ast.pure_expr) list =
        [ "tag", Pure_Copy (P_Ident (variant_tag_name ty label)) ]
      in
      let fields =
        match data with
        | None -> fields
        | Some data ->
          fields
          @ [ ( "data." ^ make_correct_ident (Label.get_name label)
              , Pure_Copy (transpile_place data) )
            ]
      in
      compound_literal
        (Value.Shape.ty_of shape)
        (fields |> List.map (fun (name, expr) -> name, C_ast.E_Pure expr))
    | V_Ty ty -> E_Native [ N_Raw "&"; N_Raw (type_info_name_for ty) ]
    | V_Fn _ | V_Generic _ -> fail "unreachable, we should never compile fns as consts"
    | V_NativeFn f -> fail "transpiling native fn %S at %s" f.name __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ ->
      (* TODO maybe? *)
      E_Pure Pure_Unit
    | V_ImplicitContext _ -> failwith __LOC__
    | V_CompilerScope _ -> E_Pure Pure_Unit
    | V_Opaque _ -> failwith __LOC__
    | V_Blocked _ -> failwith __LOC__
    | V_Error -> failwith __LOC__

  and assign (assignee : Types.assignee_expr) (pure_place_expr : C_ast.place_expr) : unit =
    match assignee.shape with
    | Types.A_Placeholder -> ()
    | Types.A_Unit -> ()
    | Types.A_Tuple { guaranteed_anonymous = _; parts } ->
      let unnamed_idx = ref 0 in
      parts
      |> List.iter (fun (part : Types.assignee_expr Types.tuple_part_of) ->
        match part with
        | Field { label_span = _; label; field } ->
          let member : Tuple.member =
            match label with
            | None ->
              let member = Tuple.Member.Index !unnamed_idx in
              unnamed_idx := !unnamed_idx + 1;
              member
            | Some label -> Tuple.Member.Name (Label.get_name label)
          in
          assign
            field
            (P_Field
               { obj = tuple_place_to_data_place pure_place_expr
               ; field = member_name member
               })
        | Unpack packed ->
          let packed_ty =
            packed.data.signature.ty
            |> Ty.await_inferred
            |> Ty.Shape.expect_tuple
            |> Option.unwrap
          in
          let var = gen_name "packed" in
          let var_ty = transpile_ty packed.data.signature.ty in
          let_var packed.data.signature.ty var None;
          if !boxed_structs
          then malloc_typed_ptr ~boxed:true packed.data.signature.ty (P_Ident var);
          packed_ty.tuple
          |> Tuple.iter (fun member (_field : Types.ty_tuple_field) ->
            let original_member : Tuple.member =
              match member with
              | Index _ ->
                let member = Tuple.Member.Index !unnamed_idx in
                unnamed_idx := !unnamed_idx + 1;
                member
              | Name name -> Name name
            in
            insert_stmt
              (S_Assign
                 { assignee =
                     P_Field { obj = tuple_place var; field = member_name member }
                 ; value =
                     E_Pure
                       (Pure_Copy
                          (P_Field
                             { obj = P_Deref (Pure_Copy pure_place_expr)
                             ; field = member_name original_member
                             }))
                 }));
          assign packed (P_Ident var))
    | Types.A_Place place -> assign_to_place place pure_place_expr
    | Types.A_Let pattern -> pattern_match pattern pure_place_expr
    | Types.A_Error -> failwith __LOC__

  and assign_to_place (place : Expr.Place.t) (value : C_ast.place_expr) =
    let ty = place.data.signature.ty in
    let place = transpile_place_expr place in
    let value = claim_c value ty in
    let value = make_pure (transpile_ty ty) value "value" in
    insert_drop ty (E_Pure (Pure_Copy place));
    insert_stmt (S_Assign { assignee = place; value = E_Pure value })

  and call_fn ~(args_is_tuple : bool) (f_expr : expr) (arg : expr) : C_ast.expr option =
    let f_ty =
      match f_expr.data.signature.ty |> Ty.await_inferred with
      | T_Fn ty -> ty
      | T_Generic _ -> failwith __LOC__
      | _ -> fail "Expected fn, got %a" Ty.print f_expr.data.signature.ty
    in
    let f = transpile_expr f_expr in
    let args : C_ast.pure_expr list =
      if args_is_tuple
      then (
        let args : C_ast.pure_expr tuple ref = ref Tuple.empty in
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
             args
             := !args
                |> Tuple.add
                     name
                     (C_ast.Pure_Copy
                        (transpile_value (Interpreter.read_place ~span field.place))))
         | E_Tuple { parts; _ } ->
           parts
           |> List.iter (function
             | (Field { label; field : expr; _ } : _ Types.tuple_part_of) ->
               let name = label |> Option.map Label.get_name in
               let arg = transpile_expr field in
               let arg = make_pure (transpile_ty field.data.signature.ty) arg "arg" in
               args := !args |> Tuple.add name arg
             | Unpack packed ->
               let packed_ty =
                 packed.data.signature.ty
                 |> Ty.await_inferred
                 |> Ty.Shape.expect_tuple
                 |> Option.unwrap
               in
               let var = gen_name "packed" in
               let_var
                 packed.data.signature.ty
                 var
                 (Some (transpile_expr packed))
                 ~drop:false;
               packed_ty.tuple
               |> Tuple.iter (fun member (_field : Types.ty_tuple_field) ->
                 args
                 := !args
                    |> Tuple.add
                         (match member with
                          | Index _ -> None
                          | Name name -> Some name)
                         (C_ast.Pure_Copy
                            (P_Field { obj = tuple_place var; field = member_name member }))))
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
                 ((member, _arg_ty) : Tuple.member * Types.ty_tuple_field)
                  : C_ast.pure_expr
                -> !args |> Tuple.get member)
        |> List.of_seq)
      else [ make_pure (transpile_ty arg.data.signature.ty) (transpile_expr arg) "arg" ]
    in
    let f, args =
      if f_ty.is_closure |> Inference.await_inferred_simple
      then (
        let f_name = gen_name "f" in
        let_var f_expr.data.signature.ty f_name (Some f);
        ( C_ast.Pure_Copy (P_Field { obj = P_Ident f_name; field = "f" })
        , [ C_ast.Pure_Copy (P_Field { obj = P_Ident f_name; field = "captured" }) ]
          @ args ))
      else make_pure (transpile_ty f_expr.data.signature.ty) f "f", args
    in
    let args =
      match f_ty.call_convention |> Inference.await_inferred_simple with
      | None -> [ C_ast.Pure_AddrOf (Effect.perform GetScope).ctx_place ] @ args
      | Some "C" -> args
      | Some other -> fail "unknown conv %S" other
    in
    let result_var = gen_name "apply_result" in
    let result_ty = transpile_ty f_ty.result in
    let apply_expr : C_ast.expr = E_Apply { f; args } in
    (match result_ty with
     | T_Unit | T_Void -> insert_stmt (S_Expr apply_expr)
     | _ -> let_var f_ty.result result_var (Some apply_expr));
    insert_stmt
      (S_If
         { cond = E_Native [ N_Raw "are_we_unwinding()" ]
         ; then_case =
             new_block (fun () -> (Effect.perform GetUnwindCtx).insert_unwind ())
         ; else_case = None
         });
    match result_ty with
    | T_Unit | T_Void -> None
    | _ -> Some (claim_c (P_Ident result_var) f_ty.result)

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
    match target |> Value.await_inferred with
    | V_Ty _ -> failwith __LOC__
    | V_Generic { ty; _ } ->
      let arg = construct_pattern_value_with_bindings ty.args.pattern in
      let result =
        Interpreter.instantiate
          span
          (Effect.perform CurrentFnCaptured).interpreter_state
          target
          arg
      in
      let result_ty = result |> Value.expect_ty |> Option.unwrap in
      transpile_ty result_ty
    | _ -> failwith __LOC__

  and transpile_expr (expr : expr) : C_ast.expr =
    match eval_expr expr with
    | Some e -> e
    | None -> uninitialized (transpile_ty expr.data.signature.ty)

  and context_field_name (context_ty : Types.value_context_ty) : string =
    make_string "context_%d" context_ty.id.value

  and current_context (context_ty : Types.value_context_ty) : C_ast.place_expr =
    let ctx = Effect.perform GetCtx in
    ctx.contexts <- ctx.contexts |> Id.Map.add context_ty.id context_ty;
    let scope = Effect.perform GetScope in
    P_Field { obj = scope.ctx_place; field = context_field_name context_ty }

  and claim (place : Types.place_expr) : C_ast.expr =
    let c_place = transpile_place_expr place in
    claim_c c_place place.data.signature.ty

  and claim_c (c_place : C_ast.place_expr) (ty : ty) : C_ast.expr =
    E_Apply
      { f = Pure_Copy (P_Ident (generate_claim ty)); args = [ Pure_AddrOf c_place ] }

  and execute_expr (expr : expr) : unit =
    match eval_expr expr with
    | None -> ()
    | Some e ->
      (match transpile_ty expr.data.signature.ty with
       | T_Unit | T_Void -> insert_stmt (S_Expr e)
       | _ ->
         insert_stmt (S_Comment (make_string "Drop %a" Ty.print expr.data.signature.ty));
         insert_drop expr.data.signature.ty e)

  and eval_scoped_expr (expr : expr) : C_ast.expr option =
    with_new_scope (fun () ->
      match transpile_ty expr.data.signature.ty with
      | T_Unit ->
        execute_expr expr;
        None
      | _ ->
        (match eval_expr expr with
         | None -> None
         | Some result ->
           let var = gen_name "scope_result" in
           let_var expr.data.signature.ty var (Some result) ~drop:false;
           Some (C_ast.E_Pure (Pure_Copy (P_Ident var)))))

  and eval_expr (expr : expr) : C_ast.expr option =
    Log.trace (fun log ->
      log "transpiling %a at %a" Print.print_expr_short expr Span.print expr.data.span);
    let ctx = Effect.perform GetCtx in
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    let span = expr.data.span in
    Inference.Var.setup_default_if_needed expr.data.signature.ty.var;
    try
      match expr.shape with
      | Types.E_Constant { id = _; value } ->
        Some (E_Pure (Pure_Copy (transpile_value value)))
      | Types.E_Ref { mut = _; place } ->
        Some (E_Pure (Pure_AddrOf (transpile_place_expr place)))
      | Types.E_Claim place -> Some (claim place)
      | Types.E_Then { list } ->
        List.fold_left
          (fun acc e ->
             (match acc with
              | None -> ()
              | Some prev -> insert_stmt (S_Expr prev));
             eval_expr e)
          None
          list
      | Types.E_Stmt { expr } ->
        execute_expr expr;
        None
      | Types.E_Scope { expr } -> eval_scoped_expr expr
      | Types.E_Fn { ty = f_ty; def; _ } ->
        let is_closure = f_ty.is_closure |> Inference.await_inferred_simple in
        let call_convention = f_ty.call_convention |> Inference.await_inferred_simple in
        if is_closure
        then (
          let transpiled_fn =
            transpile_fn ~call_convention ~is_closure ~captured:None def
          in
          let result_var = gen_name "closure" in
          insert_stmt
            (S_DeclareVar
               { name = result_var
               ; ty = transpile_ty expr.data.signature.ty
               ; value = None
               });
          let captured_arg : C_ast.expr =
            match transpiled_fn.captured_ty_name with
            | None -> E_Native [ N_Raw "NULL" ]
            | Some captured_ty_name ->
              let gc = true in
              let captured_var_name = gen_name "captured" in
              (* TODO type info for captured *)
              let_c_var
                (T_Ptr (T_Named captured_ty_name))
                captured_var_name
                (Some
                   (E_Native
                      [ N_Raw "malloc(sizeof("; N_Raw captured_ty_name; N_Raw "))" ]));
              let captured_place : C_ast.place_expr =
                P_Deref (Pure_Copy (P_Ident captured_var_name))
              in
              transpiled_fn.captured
              |> Id.Map.iter (fun _id (binding : binding) ->
                insert_stmt
                  (S_Assign
                     { assignee =
                         P_Field { obj = captured_place; field = binding_name binding }
                     ; value =
                         (if transpiled_fn.is_move
                          then claim_c (lookup_binding binding) binding.ty
                          else E_Pure (Pure_AddrOf (lookup_binding binding)))
                     }));
              insert_stmt
                (S_Assign
                   { assignee =
                       P_Field { obj = P_Ident result_var; field = "captured_TypeInfo" }
                   ; value =
                       E_Pure (Pure_AddrOf (P_Ident (captured_ty_name ^ "_TypeInfo")))
                   });
              if gc
              then E_Pure (Pure_Copy (P_Ident captured_var_name))
              else E_Pure (Pure_AddrOf (P_Ident captured_var_name))
          in
          insert_stmt
            (S_Assign
               { assignee = P_Field { obj = P_Ident result_var; field = "captured" }
               ; value = captured_arg
               });
          insert_stmt
            (S_Assign
               { assignee = P_Field { obj = P_Ident result_var; field = "f" }
               ; value = E_Pure (Pure_Copy (P_Ident transpiled_fn.name))
               });
          Some (E_Pure (Pure_Copy (P_Ident result_var))))
        else (
          let transpiled_fn =
            transpile_fn ~call_convention ~is_closure ~captured:None def
          in
          Some (E_Pure (Pure_Copy (P_Ident transpiled_fn.name))))
      | Types.E_Generic { def; _ } ->
        (* TODO memoization? *)
        failwith __LOC__
        (* Fn (transpile_fn ~captured:None def) *)
      | Types.E_Tuple { guaranteed_anonymous = _; parts } ->
        let var_name = gen_name "tuple" in
        let var_ty = transpile_ty expr.data.signature.ty in
        insert_stmt (S_DeclareVar { name = var_name; ty = var_ty; value = None });
        if !boxed_structs
        then malloc_typed_ptr ~boxed:true expr.data.signature.ty (P_Ident var_name);
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
              (S_Assign
                 { assignee = P_Field { obj = tuple_place var_name; field = field_name }
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
            let_var packed.data.signature.ty packed_name (Some (transpile_expr packed));
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
                (S_Assign
                   { assignee =
                       P_Field
                         { obj = tuple_place var_name
                         ; field = member_name assignee_member
                         }
                   ; value =
                       E_Pure
                         (Pure_Copy
                            (P_Field
                               { obj = tuple_place packed_name
                               ; field = member_name member
                               }))
                   })));
        Some (E_Pure (Pure_Copy (P_Ident var_name)))
      | Types.E_Variant { label; value; _ } ->
        let value =
          match value with
          | Some value -> transpile_expr value
          | None -> E_Pure Pure_Unit
        in
        Some
          (let var = gen_name "variant" in
           insert_stmt
             (S_DeclareVar
                { name = var; ty = transpile_ty expr.data.signature.ty; value = None });
           insert_stmt
             (S_Assign
                { assignee = P_Field { obj = P_Ident var; field = "tag" }
                ; value =
                    E_Pure
                      (Pure_Copy (P_Ident (variant_tag_name expr.data.signature.ty label)))
                });
           insert_stmt
             (S_Assign
                { assignee =
                    P_Field
                      { obj = P_Field { obj = P_Ident var; field = "data" }
                      ; field = make_correct_ident (Label.get_name label)
                      }
                ; value
                });
           E_Pure (Pure_Copy (P_Ident var)))
      | Types.E_Apply { f; arg } -> call_fn ~args_is_tuple:true f arg
      | Types.E_InstantiateGeneric _ ->
        let value = Interpreter.eval interpreter expr in
        Some (E_Pure (Pure_Copy (transpile_value value)))
      | Types.E_Assign { assignee; value } ->
        assign assignee (transpile_place_expr value);
        None
      | Types.E_Ty _ -> failwith __LOC__
      | Types.E_Newtype _ -> Some (E_Pure Pure_Unit)
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
              | Raw s -> N_Raw s
              | TyExpr e ->
                N_Raw (Interpreter.eval_ty interpreter e |> transpile_ty |> ty_to_string)
              | Expr e ->
                N_Interpolated
                  (make_pure
                     (transpile_ty e.data.signature.ty)
                     (transpile_expr e)
                     "interpolated"))
          in
          Some (E_Native parts))
      | Types.E_Module { def; bindings } ->
        let var = gen_name "module" in
        let binding_module_map = ref (Effect.perform GetBindingModuleMap) in
        bindings
        |> List.iter (fun (binding : binding) ->
          binding_module_map := !binding_module_map |> Id.Map.add binding.id var);
        let binding_module_map = !binding_module_map in
        (try
           let var_ty = transpile_ty expr.data.signature.ty in
           let_var expr.data.signature.ty var None ~drop:false;
           let module_place : C_ast.place_expr = ident_place var in
           if !boxed_structs
           then malloc_typed_ptr ~boxed:true expr.data.signature.ty module_place;
           execute_expr def;
           Some (E_Pure (Pure_Copy module_place))
         with
         | effect GetBindingModuleMap, k -> Effect.continue k binding_module_map)
      | Types.E_UseDotStar { bindings; used } ->
        (* TODO not needed? *)
        (* let stmts : C_ast.expr Dynarray.t = Dynarray.create () in *)
        (* let used_var = gen_name "used" in *)
        (* let_var (transpile_ty used.data.signature.ty) used_var (transpile_expr used); *)
        (* let used : C_ast.place_expr = P_Ident used_var in *)
        (* bindings *)
        (* |> List.iter (fun (binding : binding) -> *)
        (*   let_var *)
        (*     (transpile_ty binding.ty) *)
        (*     (binding_name binding) *)
        (*     (Pure_Copy (P_Field { obj = used; field = binding.name.name }))); *)
        None
      | Types.E_If { cond; then_case; else_case } ->
        let cond = transpile_expr cond in
        let var = gen_name "if_result" in
        let then_case =
          new_block (fun () ->
            insert_stmt
              (S_Assign { assignee = P_Ident var; value = transpile_expr then_case }))
        in
        let else_case =
          new_block (fun () ->
            insert_stmt
              (S_Assign { assignee = P_Ident var; value = transpile_expr else_case }))
        in
        insert_stmt
          (S_DeclareVar
             { name = var; ty = transpile_ty expr.data.signature.ty; value = None });
        insert_stmt (S_If { cond; then_case; else_case = Some else_case });
        Some (E_Pure (Pure_Copy (P_Ident var)))
      | Types.E_And { lhs; rhs } ->
        let result = gen_name "and_result" in
        let_c_var
          (T_Raw { c = "Bool"; is_primitive = true })
          result
          (Some (transpile_expr lhs));
        insert_stmt
          (S_If
             { cond = E_Pure (Pure_Copy (P_Ident result))
             ; then_case =
                 new_block (fun () ->
                   insert_stmt
                     (S_Assign { assignee = P_Ident result; value = transpile_expr rhs }))
             ; else_case = None
             });
        Some (E_Pure (Pure_Copy (P_Ident result)))
      | Types.E_Or { lhs; rhs } ->
        let result = gen_name "or_result" in
        let_c_var
          (T_Raw { c = "Bool"; is_primitive = true })
          result
          (Some (transpile_expr lhs));
        insert_stmt
          (S_If
             { cond = E_Pure (Pure_Not (Pure_Copy (P_Ident result)))
             ; then_case =
                 new_block (fun () ->
                   insert_stmt
                     (S_Assign { assignee = P_Ident result; value = transpile_expr rhs }))
             ; else_case = None
             });
        Some (E_Pure (Pure_Copy (P_Ident result)))
      | Types.E_Match { value; branches } ->
        let value_var = gen_name "matched" in
        (* TODO panic(not exhaustive) *)
        let_c_var
          (T_Ptr (transpile_ty value.data.signature.ty))
          value_var
          (Some (E_Pure (Pure_AddrOf (transpile_place_expr value))));
        let result_var = gen_name "match_result" in
        let end_of_match_label = gen_name "end_of_match" in
        insert_stmt
          (S_DeclareVar
             { name = result_var; ty = transpile_ty expr.data.signature.ty; value = None });
        branches
        |> List.iter (fun ({ pattern; body } : Types.expr_match_branch) ->
          insert_stmt
            (S_If
               { cond =
                   E_Pure (does_match pattern (P_Deref (Pure_Copy (P_Ident value_var))))
               ; then_case =
                   new_block
                     (fun () ->
                        pattern_match pattern (P_Deref (Pure_Copy (P_Ident value_var)));
                        insert_stmt
                          (S_Assign
                             { assignee = P_Ident result_var
                             ; value = transpile_expr body
                             }))
                     ~after_cleanup:(fun () ->
                       insert_stmt (S_Goto { label = end_of_match_label }))
               ; else_case = None
               }));
        insert_stmt (S_Native [ N_Raw "Kast_match_non_exhaustive()" ]);
        insert_stmt (S_GotoLabel end_of_match_label);
        Some (E_Pure (Pure_Copy (P_Ident result_var)))
      | Types.E_QuoteAst _ -> failwith __LOC__
      | Types.E_Loop { body } ->
        insert_stmt (S_For { body = new_block (fun () -> execute_expr body) });
        None
      | Types.E_Unwindable { token; body } ->
        let label_on_unwind = gen_name "on_unwind" in
        let label_result = gen_name "unwindable_result" in
        let token_var = gen_name "unwindable_token" in
        let token_ty = ty_repr token.data.signature.ty in
        let_var
          token_ty
          token_var
          (Some
             (compound_literal
                token_ty
                [ "raw", C_ast.E_Native [ N_Raw "RawUnwindToken_new()" ] ]));
        let token_ref_var = gen_name "unwindable_token_ref" in
        let_c_var
          (T_Ptr (transpile_ty token_ty))
          token_ref_var
          (Some (E_Pure (Pure_AddrOf (P_Ident token_var))));
        pattern_match token (P_Ident token_ref_var);
        let result_place : C_ast.place_expr =
          P_Field { obj = tuple_place token_var; field = "value" }
        in
        let old_unwind_ctx = Effect.perform GetUnwindCtx in
        let unwind_ctx : unwind_ctx =
          { insert_unwind = (fun () -> insert_stmt (S_Goto { label = label_on_unwind }))
          ; cleanup_scope_without_unwind = (fun () -> ())
          }
        in
        (try
           match eval_expr body with
           | None -> ()
           | Some result ->
             insert_stmt (S_Assign { assignee = result_place; value = result })
         with
         | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx);
        insert_stmt (S_Goto { label = label_result });
        insert_stmt (S_GotoLabel label_on_unwind);
        insert_stmt
          (S_If
             { cond =
                 E_Apply
                   { f = Pure_Native [ N_Raw "are_we_unwinding_with" ]
                   ; args =
                       [ Pure_Copy
                           (P_Field { obj = tuple_place token_var; field = "raw" })
                       ]
                   }
             ; then_case =
                 new_block (fun () ->
                   insert_stmt (S_Native [ N_Raw "stop_unwinding()" ]);
                   insert_stmt (S_Goto { label = label_result }))
             ; else_case = Some (new_block (fun () -> old_unwind_ctx.insert_unwind ()))
             });
        insert_stmt (S_GotoLabel label_result);
        Some (claim_c result_place expr.data.signature.ty)
        (* let token_ident = gen_name "token" in *)
        (* Unwindable *)
        (*   { token_ident *)
        (*   ; body = Then [ pattern_match token (P_Ident token_ident); transpile_expr body ] *)
        (*   } *)
      | Types.E_Unwind { token; value } ->
        let token_var = gen_name "token" in
        let_var token.data.signature.ty token_var (Some (transpile_expr token));
        (* token->value = value *)
        insert_stmt
          (S_Assign
             { assignee =
                 P_Field
                   { obj = P_Deref (Pure_Copy (tuple_place token_var)); field = "value" }
             ; value = transpile_expr value
             });
        (* currently_unwinding = token->raw *)
        insert_stmt
          (S_Expr
             (E_Apply
                { f = Pure_Native [ N_Raw "start_unwinding" ]
                ; args =
                    [ Pure_Copy
                        (P_Field
                           { obj = P_Deref (Pure_Copy (tuple_place token_var))
                           ; field = "raw"
                           })
                    ]
                }));
        (Effect.perform GetUnwindCtx).insert_unwind ();
        None
      | Types.E_InjectContext { context_ty; value } ->
        let value = transpile_expr value in
        let old_var = gen_name "old_ctx" in
        let ctx_place = current_context context_ty in
        let_var context_ty.ty old_var (Some (E_Pure (Pure_Copy ctx_place))) ~drop:false;
        insert_stmt (S_Assign { assignee = ctx_place; value });
        defer (fun () ->
          insert_drop context_ty.ty (E_Pure (Pure_Copy ctx_place));
          insert_stmt
            (S_Assign
               { assignee = ctx_place; value = E_Pure (Pure_Copy (P_Ident old_var)) }));
        None
      | Types.E_LetRefContext new_ref ->
        let new_ctx_var = gen_name "ctx" in
        let_c_var
          (T_Raw { c = "Context*"; is_primitive = false })
          new_ctx_var
          (Some (transpile_expr new_ref));
        (Effect.perform GetScope).ctx_place <- P_Deref (Pure_Copy (P_Ident new_ctx_var));
        None
      | Types.E_ImplCast _ -> None
      | Types.E_Cast _ ->
        let value = Interpreter.eval interpreter expr in
        (* TODO same as const *)
        Some (E_Pure (Pure_Copy (transpile_value value)))
      | Types.E_TargetDependent target_dependent ->
        let branch =
          Kast_interpreter.find_target_dependent_branch
            interpreter
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

  let postprocess () =
    let ctx = Effect.perform GetCtx in
    (* let fns = program.fns |> StringMap.mapi postprocess_fn in *)
    ctx.type_infos
    |> CTyMap.iter (fun (ty : C_ast.ty) (type_info : type_info) ->
      Dynarray.add_last
        ctx.statics
        { ty = T_Raw { c = "TypeInfo"; is_primitive = false }
        ; name = type_info.type_info_name
        ; comment = None
        });
    let rec construct_ty_def_type_info (kast_ty : ty) (name : string) (def : C_ast.ty_def)
      : C_ast.expr
      =
      match def.shape with
      | C_ast.TD_Alias ty -> construct_ty_type_info kast_ty ty
      | _ ->
        c_compound_literal
          (T_Raw { c = "TypeInfo"; is_primitive = false })
          (([ "name", C_ast.Pure_Literal (L_String name)
            ; "alignment", C_ast.Pure_Native [ N_Raw "alignof("; N_Raw name; N_Raw ")" ]
            ; "size", Pure_Native [ N_Raw "sizeof("; N_Raw name; N_Raw ")" ]
            ; "stride", Pure_Native [ N_Raw "sizeof("; N_Raw name; N_Raw ")" ]
            ; ( "dbg_write"
              , Pure_Copy (P_Ident (generate_dbg_write kast_ty ^ "_type_erased")) )
            ; "drop", Pure_Copy (P_Ident (generate_drop kast_ty ^ "_type_erased"))
            ; "claim", Pure_Copy (P_Ident (generate_claim kast_ty ^ "_type_erased"))
            ]
            @
            if !allocation_stats
            then
              [ ( "allocation_stats"
                , C_ast.Pure_Native [ N_Raw "Kast_type_allocation_stats_new()" ] )
              ]
            else [])
           |> List.map (fun (name, expr) -> name, C_ast.E_Pure expr))
    and construct_ty_type_info (kast_ty : ty) (ty : C_ast.ty) =
      let ctx = Effect.perform GetCtx in
      let raw (raw_ty : string) : C_ast.expr =
        if raw_ty = "String"
        then E_Native [ N_Raw "String_TypeInfo" ]
        else
          c_compound_literal
            (T_Raw { c = "TypeInfo"; is_primitive = false })
            (([ "name", C_ast.Pure_Literal (L_String (make_string "%a" Ty.print kast_ty))
              ; ( "alignment"
                , C_ast.Pure_Native [ N_Raw "alignof("; N_Raw raw_ty; N_Raw ")" ] )
              ; "size", Pure_Native [ N_Raw "sizeof("; N_Raw raw_ty; N_Raw ")" ]
              ; "stride", Pure_Native [ N_Raw "sizeof("; N_Raw raw_ty; N_Raw ")" ]
              ; ( "dbg_write"
                , Pure_Copy (P_Ident (generate_dbg_write kast_ty ^ "_type_erased")) )
              ; "drop", Pure_Copy (P_Ident (generate_drop kast_ty ^ "_type_erased"))
              ; "claim", Pure_Copy (P_Ident (generate_claim kast_ty ^ "_type_erased"))
              ]
              @
              if !allocation_stats
              then
                [ ( "allocation_stats"
                  , C_ast.Pure_Native [ N_Raw "Kast_type_allocation_stats_new()" ] )
                ]
              else [])
             |> List.map (fun (name, expr) -> name, C_ast.E_Pure expr))
      in
      match ty with
      | T_Unit -> raw "Unit"
      | T_Raw { c = raw_ty; is_primitive } -> raw raw_ty
      | T_Named name ->
        let def =
          ctx.types
          |> StringMap.find_opt name
          |> Option.unwrap_or_else (fun () -> failwith __LOC__)
        in
        construct_ty_def_type_info kast_ty name def
      | T_Ptr _ -> raw "void*" (* TODO void* technically works but should fix *)
      | T_Void -> fail "tried to create type info for void?"
    in
    let init_type_infos_fn : C_ast.fn_def =
      { args = []
      ; result_ty = T_Void
      ; comment = None
      ; body =
          new_block (fun () ->
            ctx.raw_type_infos |> Dynarray.iter (fun f -> f ());
            ctx.type_infos
            |> CTyMap.iter (fun ty type_info ->
              insert_stmt
                (S_Assign
                   { assignee = P_Ident type_info.type_info_name
                   ; value = construct_ty_type_info type_info.kast_ty ty
                   })))
      }
    in
    ctx.fns <- ctx.fns |> StringMap.add "Kast_init_user_type_infos" init_type_infos_fn
  ;;
end

let transpile_expr (interpreter : Interpreter.state) (expr : expr) : C_ast.program =
  Kast_inference_completion.enable := true;
  Kast_inference_completion.complete_compiled Expr expr;
  let runtime_defined_closure_types = ref StringListMap.empty in
  let runtime_defined_list_types = ref StringMap.empty in
  let runtime_defined_box_types = ref StringMap.empty in
  let runtime_source = [%include_file "runtime.c"] in
  runtime_source
  |> String.split_on_char '\n'
  |> List.iter (fun s ->
    let check_defined_closure () =
      let* s = s |> String.strip_prefix ~prefix:"define_closure_type(" in
      let* s = s |> String.strip_suffix ~suffix:");" in
      let args = s |> String.split_on_char ',' |> List.map String.trim in
      match args with
      | name :: args ->
        Log.trace (fun log ->
          log "Found defined closure type %a = %s" (List.print String.print) args name);
        runtime_defined_closure_types
        := !runtime_defined_closure_types |> StringListMap.add args name;
        Some ()
      | _ -> failwith __LOC__
    in
    let check_defined_list () =
      let* s = s |> String.strip_prefix ~prefix:"define_ArrayList(" in
      let* arg = s |> String.strip_suffix ~suffix:");" in
      runtime_defined_list_types
      := !runtime_defined_list_types |> StringMap.add arg ("ArrayList_" ^ arg);
      Some ()
    in
    let check_defined_box () =
      let* s = s |> String.strip_prefix ~prefix:"define_Box(" in
      let* arg = s |> String.strip_suffix ~suffix:");" in
      runtime_defined_list_types
      := !runtime_defined_list_types |> StringMap.add arg ("Box_" ^ arg);
      Some ()
    in
    let _ : unit option = check_defined_closure () in
    let _ : unit option = check_defined_list () in
    let _ : unit option = check_defined_box () in
    ());
  let ctx : ctx =
    { target = { name = "c" }
    ; statics = Dynarray.create ()
    ; captured_values = ValueMap.empty
    ; captured_types = ValueMap.empty
    ; drop_fns = ValueMap.empty
    ; dbg_write_fns = ValueMap.empty
    ; claim_fns = ValueMap.empty
    ; types =
        StringMap.of_list
          (([ "Unit"
            ; "Byte"
            ; "Bool"
            ; "Int32"
            ; "UInt32"
            ; "Float32"
            ; "Float64"
            ; "Char"
            ; "Int64"
            ; "UInt64"
            ; "void"
            ]
            |> List.map (fun name : (string * C_ast.ty_def) ->
              ( name
              , { shape = C_ast.TD_RuntimeDefined { is_primitive = true }
                ; comment = None
                } )))
           @ ([ "String"; "TypeInfo"; "Context"; "StringView"; "Kast_Formatter" ]
              |> List.map (fun name : (string * C_ast.ty_def) ->
                ( name
                , { shape = C_ast.TD_RuntimeDefined { is_primitive = false }
                  ; comment = None
                  } ))))
    ; fns = StringMap.empty
    ; init_statics = { stmts = [] }
    ; includes = StringSet.empty
    ; contexts = Id.Map.empty
    ; type_infos = CTyMap.empty
    ; raw_type_infos = Dynarray.create ()
    ; runtime_defined_closure_types = !runtime_defined_closure_types
    ; runtime_defined_list_types = !runtime_defined_list_types
    ; runtime_defined_box_types = !runtime_defined_box_types
    }
  in
  let captured_scope = Interpreter.Scope.init ~recursive:false ~parent:None ~span in
  (* let captured_scope = interpreter.scope in *)
  let captured : current_captured =
    { interpreter_scope = captured_scope
    ; bindings = Id.Map.empty
    ; interpreter_state =
        { interpreter with
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
        (fun () ->
          Impl.insert_stmt
            (S_Return (E_Pure (Pure_Literal (L_Int32 (Int32.of_int (-1)))))))
    ; cleanup_scope_without_unwind = (fun () -> ())
    }
  in
  let scope : scope = { ctx_place = P_Ident ctx_var } in
  try
    let main : C_ast.fn_def =
      { args =
          [ { name = "argc"; ty = T_Raw { c = "int"; is_primitive = true } }
          ; { name = "argv"; ty = T_Raw { c = "char**"; is_primitive = false } }
          ]
      ; result_ty = T_Raw { c = "int"; is_primitive = true }
      ; body =
          Impl.new_block
            (fun () ->
               Impl.insert_stmt (S_Native [ N_Raw "Kast_init(argc, argv)" ]);
               Impl.let_c_var (T_Named "Context") ctx_var None;
               Impl.insert_stmt
                 (S_Expr
                    (E_Apply { f = Pure_Copy (P_Ident "KAST_init_statics"); args = [] }));
               Impl.execute_expr expr;
               unwind_ctx.cleanup_scope_without_unwind ())
            ~after_cleanup:(fun () ->
              Impl.insert_stmt
                (S_Return (E_Pure (Pure_Literal (L_Int32 (Int32.of_int 0))))))
      ; comment = None
      }
    in
    let init_statics : C_ast.fn_def =
      { args = []
      ; result_ty = T_Raw { c = "void"; is_primitive = true }
      ; body = ctx.init_statics.stmts
      ; comment = None
      }
    in
    ctx.fns <- ctx.fns |> StringMap.add "main" main;
    ctx.fns <- ctx.fns |> StringMap.add "KAST_init_statics" init_statics;
    context_ty_def
    := Some
         ({ shape =
              TD_Struct
                (ctx.contexts
                 |> Id.Map.to_list
                 |> List.map (fun ((_id, context_ty) : Id.t * Types.value_context_ty) ->
                   Impl.context_field_name context_ty, Impl.transpile_ty context_ty.ty)
                 |> StringMap.of_list)
          ; comment = Some "implicit context"
          }
          : C_ast.ty_def);
    ctx.types
    <- ctx.types
       |> StringMap.add "Context" (!context_ty_def |> Option.unwrap)
       |> StringMap.union
            (fun _ a _ -> Some a)
            (ctx.runtime_defined_closure_types
             |> StringListMap.to_list
             |> List.map (fun (_, name) ->
               ( name
               , ({ shape = TD_RuntimeDefined { is_primitive = false }; comment = None }
                  : C_ast.ty_def) ))
             |> StringMap.of_list)
       |> StringMap.union
            (fun _ a _ -> Some a)
            (ctx.runtime_defined_list_types
             |> StringMap.to_list
             |> List.map (fun (_, name) ->
               ( name
               , ({ shape = TD_RuntimeDefined { is_primitive = false }; comment = None }
                  : C_ast.ty_def) ))
             |> StringMap.of_list);
    Impl.postprocess ();
    { fns = ctx.fns
    ; statics = ctx.statics |> Dynarray.to_list
    ; includes = ctx.includes
    ; types = ctx.types
    }
  with
  | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx
  | effect GetScope, k -> Effect.continue k scope
  | effect CurrentFnCaptured, k -> Effect.continue k captured
  | effect GetCtx, k -> Effect.continue k ctx
  | effect GetBindingModuleMap, k -> Effect.continue k Id.Map.empty
;;
