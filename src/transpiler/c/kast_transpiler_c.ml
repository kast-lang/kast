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

type gc_mode =
  | Full
  | EscapeAnalyze
  | RuntimeBorrowChecker
  | Disabled

let gc_mode = ref EscapeAnalyze
let typed_gc = ref true
let allocation_stats = ref false

let is_full_gc () =
  match !gc_mode with
  | Full -> true
  | _ -> false
;;

let boxed_structs = ref false

let tuple_place_to_data_place place : C_ast.place_expr =
  if !boxed_structs then Deref (Claim place) else place
;;

let tuple_place var : C_ast.place_expr = tuple_place_to_data_place (Ident var)

type block = { mutable stmts : C_ast.stmt list }

module StringListMap = Map.Make (struct
    type t = string list

    let compare = List.compare String.compare
  end)

type ctx =
  { target : Types.value_target
  ; mutable includes : StringSet.t
  ; mutable types : C_ast.ty_def StringMap.t
  ; mutable type_infos : string CTyMap.t
  ; mutable fns : C_ast.fn_def StringMap.t
  ; mutable statics : C_ast.static Dynarray.t
  ; mutable captured_values : string ValueMap.t
  ; mutable captured_types : string ValueMap.t
  ; mutable drop_fns : string ValueMap.t
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

type ty_repr = C_ast.ty

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
    | Named name -> variant_tag_name_impl name label
    | _ -> failwith __LOC__

  and variant_tag_name_impl (ty_name : string) (label : Label.t) : string =
    make_correct_ident (ty_name ^ "_" ^ Label.get_name label)

  and insert_stmt (stmt : C_ast.stmt) : unit =
    let block = Effect.perform GetCurrentBlock in
    block.stmts <- block.stmts @ [ stmt ]

  and ty_to_string (ty : ty_repr) : string =
    let ctx = Effect.perform GetCtx in
    match ty with
    | T_Unit -> "Unit"
    | T_Raw { c; is_primitive = _ } -> c
    | Named name ->
      name
      (* (match ctx.types |> StringMap.find_opt name with *)
      (*  | Some { shape = Alias ty; _ } -> ty_to_string ty *)
      (*  | Some _ -> name *)
      (*  | None -> fail "type %S is named but not in ctx.types" name) *)
    | Ptr p ->
      let name = ty_to_string p ^ "_Ptr" in
      let ty_def : C_ast.ty_def = { shape = Alias ty; comment = None } in
      ctx.types <- ctx.types |> StringMap.add name ty_def;
      name
    | Void -> "void"

  and type_info_name_for (ty : C_ast.ty) : string =
    let ctx = Effect.perform GetCtx in
    match ctx.type_infos |> CTyMap.find_opt ty with
    | Some name -> name
    | None ->
      let name = gen_name (make_string "%s_TypeInfo" (ty_to_string ty)) in
      ctx.type_infos <- ctx.type_infos |> CTyMap.add ty name;
      name

  and malloc_typed_ptr ~(boxed : bool) (ty : C_ast.ty) (ptr : C_ast.place_expr) : unit =
    let ty =
      if boxed
      then (
        match ty with
        | Ptr ty -> ty
        | _ -> failwith __LOC__)
      else ty
    in
    let ty_info_name = type_info_name_for ty in
    insert_stmt
      (Assign
         { assignee = ptr
         ; value = E_Native { parts = [ Raw ("Kast_allocate(&" ^ ty_info_name ^ ")") ] }
         })

  and declare_var ~(gc : bool) (ty : C_ast.ty) (name : string) ~(drop : string option)
    : unit
    =
    if gc
    then (
      insert_stmt (DeclareVar { name; ty = Ptr ty });
      malloc_typed_ptr ~boxed:false ty (Ident name))
    else insert_stmt (DeclareVar { name; ty });
    match drop with
    | None -> ()
    | Some drop ->
      defer (fun () ->
        insert_stmt
          (Expr (Apply { f = Claim (Ident drop); args = [ Claim (Ident name) ] })))

  and insert_drop (thing : C_ast.expr) (ty : ty) =
    let name = gen_name "to_drop" in
    let c_ty = transpile_ty ty in
    match c_ty with
    | T_Unit -> insert_stmt (Expr thing)
    | _ ->
      let_var ~gc:false c_ty name thing ~drop:None;
      insert_stmt
        (Expr
           (Apply { f = Claim (Ident (generate_drop ty)); args = [ AddrOf (Ident name) ] }))

  and let_var
        ~(gc : bool)
        (ty : C_ast.ty)
        (name : string)
        (value : C_ast.expr)
        ~(drop : string option)
    : unit
    =
    declare_var ~gc ty name ~drop;
    insert_stmt
      (Assign
         { assignee = (if gc then Deref (Claim (Ident name)) else Ident name); value })

  and compound_literal ~kast (ty : C_ast.ty) (fields : (string * C_ast.expr) list)
    : C_ast.expr
    =
    let result_name = gen_name "compound" in
    insert_stmt (DeclareVar { name = result_name; ty });
    let boxed = kast && !boxed_structs in
    if boxed then malloc_typed_ptr ~boxed ty (Ident result_name);
    fields
    |> List.iter (fun (name, value) ->
      insert_stmt
        (Assign
           { assignee =
               Field
                 { obj =
                     (if boxed
                      then Deref (Claim (Ident result_name))
                      else Ident result_name)
                 ; field = name
                 }
           ; value
           }));
    Claim (Ident result_name)

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

  and new_block (f : unit -> unit) : C_ast.block =
    let block = { stmts = [] } in
    try
      with_new_scope f;
      block.stmts
    with
    | effect GetCurrentBlock, k -> Effect.continue k block

  and declare_binding (binding : binding) (value : C_ast.expr) : unit =
    match Effect.perform GetBindingModuleMap |> Id.Map.find_opt binding.id with
    | Some _ -> insert_stmt (Assign { assignee = lookup_binding binding; value })
    | None ->
      let ident = binding_name binding in
      let ty = transpile_ty binding.ty in
      let drop = generate_drop binding.ty in
      let_var ~gc:(is_full_gc ()) ty ident value ~drop:(Some drop)

  and ident_place (name : string) : C_ast.place_expr = get_actual_place (Ident name)

  and get_actual_place (place : C_ast.place_expr) : C_ast.place_expr =
    match !gc_mode with
    | Full -> Deref (Claim place)
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
            Field
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
       | Types.PE_Scope expr -> with_new_scope (fun () -> transpile_place_expr expr)
       | Types.PE_Field { obj; field; field_span = _ } ->
         let field =
           match field with
           | Types.Index i -> member_name (Index i)
           | Types.Name label -> member_name (Name (Label.get_name label))
           | Types.Expr _ -> failwith __LOC__
         in
         let obj = transpile_place_expr obj in
         Field { obj = (if !boxed_structs then Deref (Claim obj) else obj); field }
       | Types.PE_Deref expr ->
         (match expr.data.signature.ty |> Ty.await_inferred with
          | T_Box _ ->
            Deref (Claim (Field { obj = transpile_place_expr expr; field = "value" }))
          | _ -> Deref (Claim (transpile_place_expr expr)))
       | Types.PE_Temp expr ->
         let var = gen_name "temp" in
         let_var
           ~gc:false
           (transpile_ty expr.data.signature.ty)
           var
           (transpile_expr expr)
           ~drop:(Some (generate_drop expr.data.signature.ty));
         Ident var
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

  and resolve_ty_aliases (ty : C_ast.ty) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    match ty with
    | T_Unit -> T_Unit
    | T_Raw s -> T_Raw s
    | Ptr pointee -> Ptr (resolve_ty_aliases pointee)
    | Void -> Void
    | Named name ->
      (match ctx.types |> StringMap.find_opt name with
       | Some def ->
         (match def.shape with
          | RuntimeDefined -> ty
          | DEF_Raw _ -> ty
          | Enum _ | Struct _ | Union _ -> ty
          | Fn _ -> ty
          | Alias ty -> resolve_ty_aliases ty)
       | None -> ty (* fail "named type %S is not found in ctx.types???" name *))

  and generate_claim (ty : ty) : string =
    let ctx = Effect.perform GetCtx in
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    Inference.Var.setup_default_if_needed ty.var;
    let ty =
      Interpreter.Substitute_bindings.sub_ty
        ~span
        ~state:(Interpreter.sub_here interpreter)
        ty
    in
    Inference.Var.setup_default_if_needed ty.var;
    ty |> Ty.await_inferred |> ignore;
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
               let name = gen_name (make_string "claim_%a" Ty.print ty) in
               do_prepend := true;
               name
           in
           claim_name := Some name;
           Some name);
      let claim_name = !claim_name |> Option.get in
      if !do_prepend
      then (
        let claim_impl = generate_claim_impl ty in
        ctx.fns <- ctx.fns |> StringMap.add claim_name claim_impl);
      claim_name

  and generate_claim_impl (ty : ty) : C_ast.fn_def =
    let ty_ty = ty in
    let arg_name = "place" in
    { comment = Some (make_string "claim for %a" Ty.print ty)
    ; args = [ { name = arg_name; ty = Ptr (transpile_ty ty) } ]
    ; result_ty = transpile_ty ty
    ; body =
        new_block (fun () ->
          let ty =
            match ty.var |> Inference.Var.inferred_opt with
            | None -> fail "can't generate_claim_impl for not inferred"
            | Some ty -> ty
          in
          let copy : C_ast.expr = Claim (Deref (Claim (Ident arg_name))) in
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
              Apply
                { f = E_Native { parts = [ Raw "String_claim" ] }
                ; args = [ Claim (Ident arg_name) ]
                }
            | Types.T_Char -> copy
            | Types.T_Box boxed ->
              Apply
                { f =
                    E_Native
                      { parts =
                          [ Raw "Box_"
                          ; Raw (ty_to_string (transpile_ty boxed))
                          ; Raw "_claim"
                          ]
                      }
                ; args = [ Claim (Ident arg_name) ]
                }
            | Types.T_Ref _ -> copy
            | Types.T_Variant variant ->
              (match variant.variants |> Row.await_inferred_to_list with
               | [] -> copy
               | variants ->
                 let var = gen_name "claimed" in
                 declare_var ~gc:false (transpile_ty ty_ty) var ~drop:None;
                 insert_stmt
                   (Switch
                      { value =
                          Claim
                            (Field { obj = Deref (Claim (Ident arg_name)); field = "tag" })
                      ; cases =
                          variants
                          |> List.map
                               (fun
                                   ((label, data) : Label.t * Types.ty_variant_data)
                                    : C_ast.switch_case
                                  ->
                                  let tag =
                                    C_ast.Claim (Ident (variant_tag_name ty_ty label))
                                  in
                                  { value = tag
                                  ; body =
                                      new_block (fun () ->
                                        insert_stmt
                                          (Assign
                                             { assignee =
                                                 Field { obj = Ident var; field = "tag" }
                                             ; value = tag
                                             });
                                        match data.data with
                                        | None -> ()
                                        | Some data ->
                                          insert_stmt
                                            (Assign
                                               { assignee =
                                                   Field
                                                     { obj =
                                                         Field
                                                           { obj = Ident var
                                                           ; field = "data"
                                                           }
                                                     ; field =
                                                         make_correct_ident
                                                           (Label.get_name label)
                                                     }
                                               ; value =
                                                   claim_c
                                                     (Field
                                                        { obj =
                                                            Field
                                                              { obj =
                                                                  Deref
                                                                    (Claim
                                                                       (Ident arg_name))
                                                              ; field = "data"
                                                              }
                                                        ; field =
                                                            make_correct_ident
                                                              (Label.get_name label)
                                                        })
                                                     data
                                               }))
                                  })
                      ; default = None
                      });
                 Claim (Ident var))
            | Types.T_Tuple tuple ->
              compound_literal
                ~kast:true
                (transpile_ty ty_ty)
                (tuple.tuple
                 |> Tuple.to_seq
                 |> List.of_seq
                 |> List.map
                      (fun
                          ((member, field) : Tuple.member * Types.ty_tuple_field)
                           : (string * C_ast.expr)
                         ->
                         ( member_name member
                         , Apply
                             { f = Claim (Ident (generate_claim field.ty))
                             ; args =
                                 [ AddrOf
                                     (Field
                                        { obj = Deref (Claim (Ident arg_name))
                                        ; field = member_name member
                                        })
                                 ]
                             } )))
            | Types.T_List _ -> copy
            | Types.T_Ty -> copy
            | Types.T_Fn _ -> copy
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
          insert_stmt (Return result))
    }

  and generate_drop (ty : ty) : string =
    let ctx = Effect.perform GetCtx in
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    Inference.Var.setup_default_if_needed ty.var;
    let ty =
      Interpreter.Substitute_bindings.sub_ty
        ~span
        ~state:(Interpreter.sub_here interpreter)
        ty
    in
    Inference.Var.setup_default_if_needed ty.var;
    ty |> Ty.await_inferred |> ignore;
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
               let name = gen_name (make_string "drop_%a" Ty.print ty) in
               do_prepend := true;
               name
           in
           drop_name := Some name;
           Some name);
      let drop_name = !drop_name |> Option.get in
      if !do_prepend
      then (
        let drop_impl = generate_drop_impl ty in
        ctx.fns <- ctx.fns |> StringMap.add drop_name drop_impl);
      drop_name

  and generate_drop_impl (ty : ty) : C_ast.fn_def =
    let ty_ty = ty in
    let var = "value" in
    { comment = Some (make_string "Drop for %a" Ty.print ty)
    ; args = [ { name = var; ty = transpile_ty ty } ]
    ; result_ty = Void
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
              (Expr
                 (Apply
                    { f = E_Native { parts = [ Raw "String_drop" ] }
                    ; args = [ Claim (Ident var) ]
                    }))
          | Types.T_Char -> ()
          | Types.T_Box boxed ->
            insert_stmt
              (Expr
                 (Apply
                    { f =
                        E_Native
                          { parts =
                              [ Raw "Box_"
                              ; Raw (ty_to_string (transpile_ty boxed))
                              ; Raw "_drop"
                              ]
                          }
                    ; args = [ Claim (Ident var) ]
                    }))
          | Types.T_Ref _ -> ()
          | Types.T_Variant variant ->
            (match variant.variants |> Row.await_inferred_to_list with
             | [] -> ()
             | variants ->
               insert_stmt
                 (Switch
                    { value = Claim (Field { obj = Ident var; field = "tag" })
                    ; cases =
                        variants
                        |> List.map
                             (fun
                                 ((label, data) : Label.t * Types.ty_variant_data)
                                  : C_ast.switch_case
                                ->
                                { value = Claim (Ident (variant_tag_name ty_ty label))
                                ; body =
                                    new_block (fun () ->
                                      match data.data with
                                      | None -> ()
                                      | Some data ->
                                        insert_stmt
                                          (Expr
                                             (Apply
                                                { f = Claim (Ident (generate_drop data))
                                                ; args =
                                                    [ Claim
                                                        (Field
                                                           { obj =
                                                               Field
                                                                 { obj = Ident var
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
                (Expr
                   (Apply
                      { f = Claim (Ident (generate_drop field.ty))
                      ; args =
                          [ Claim (Field { obj = Ident var; field = member_name member })
                          ]
                      })))
          | Types.T_List _ -> todo
          | Types.T_Ty -> ()
          | Types.T_Fn _ -> ()
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
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    Inference.Var.setup_default_if_needed ty.var;
    let ty =
      Interpreter.Substitute_bindings.sub_ty
        ~span
        ~state:(Interpreter.sub_here interpreter)
        ty
    in
    Inference.Var.setup_default_if_needed ty.var;
    ty |> Ty.await_inferred |> ignore;
    match ty.var |> Inference.Var.inferred_opt with
    | Some (T_Blocked value) -> Ptr Void
    | _ ->
      let ty_name = ref None in
      let do_prepend = ref false in
      (* Log.info (fun log -> log "Checking in ValueMap: %a" Ty.print ty); *)
      (* let old_captured_types = ctx.captured_types in *)
      let ty_as_value = V_Ty ty |> Value.inferred ~span in
      ctx.captured_types
      <- ctx.captured_types
         |> ValueMap.update ty_as_value (fun name ->
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
        (match
           Interpreter.cast_as_module_opt
             ~span
             interpreter
             (Value.inferred ~span (V_Ty ty))
         with
         | None -> ()
         | Some ty_as_module -> transpile_value ty_as_module |> ignore);
        Log.trace (fun log -> log "prepend %a" Ty.print ty);
        let ty_def =
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
          | Some shape -> transpile_ty_shape ty_name shape
        in
        ctx.types <- ctx.types |> StringMap.add ty_name ty_def);
      resolve_ty_aliases (Named ty_name)

  and variant_tag_ty (ty_name : string) (ty : Types.ty_variant) : C_ast.ty =
    let ctx = Effect.perform GetCtx in
    let name = ty_name ^ "_TAG" in
    let variant_names =
      ty.variants
      |> Row.await_inferred_to_list
      |> List.map (fun (label, _) -> variant_tag_name_impl ty_name label)
    in
    let def : C_ast.ty_def =
      { shape = Enum (StringSet.of_list variant_names)
      ; comment = Some (make_string "Tag for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    Named name

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
      { shape = Union (StringMap.of_list variants)
      ; comment = Some (make_string "Data for %a" Print.print_ty_variant ty)
      }
    in
    ctx.types <- ctx.types |> StringMap.add name def;
    Named name

  and add_include s =
    let ctx = Effect.perform GetCtx in
    ctx.includes <- ctx.includes |> StringSet.add s

  and variant_needs_tag (ty : Types.ty_variant) : bool =
    ty.variants |> Row.await_inferred_to_list |> List.length > 0

  and uninitialized (ty : C_ast.ty) : C_ast.expr =
    match ty with
    | T_Unit -> Unit
    | _ ->
      let var = gen_name "uninitialized" in
      insert_stmt (DeclareVar { name = var; ty });
      insert_stmt
        (S_Native { parts = [ Raw ("memset(&" ^ var ^ ", 0, sizeof(" ^ var ^ "))") ] });
      Claim (Ident var)

  and ty_repr (ty : ty) : ty_repr =
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    Inference.Var.setup_default_if_needed ty.var;
    let ty =
      Interpreter.Substitute_bindings.sub_ty
        ~span
        ~state:(Interpreter.sub_here interpreter)
        ty
    in
    ty |> Ty.await_inferred |> ty_shape_repr

  and ty_shape_repr (ty : Types.ty_shape) : ty_repr =
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
      transpile_ty impl
    | _ -> fail "no repr for %a" Ty.Shape.print ty

  and transpile_ty_shape (ty_name : string) (ty : Types.ty_shape) : C_ast.ty_def =
    let ctx = Effect.perform GetCtx in
    let shape : C_ast.ty_def_shape =
      match ty with
      | Types.T_Unit -> Alias T_Unit
      | Types.T_Bool -> Alias (T_Raw { c = "Bool"; is_primitive = true })
      | Types.T_Int32 -> Alias (T_Raw { c = "Int32"; is_primitive = true })
      | Types.T_UInt32 -> Alias (T_Raw { c = "UInt32"; is_primitive = true })
      | Types.T_Int64 -> Alias (T_Raw { c = "Int64"; is_primitive = true })
      | Types.T_UInt64 -> Alias (T_Raw { c = "UInt64"; is_primitive = true })
      | Types.T_Float32 -> Alias (T_Raw { c = "Float32"; is_primitive = true })
      | Types.T_Float64 -> Alias (T_Raw { c = "Float64"; is_primitive = true })
      | Types.T_String -> Alias (T_Raw { c = "String"; is_primitive = false })
      | Types.T_StringView -> Alias (T_Raw { c = "StringView"; is_primitive = false })
      | Types.T_Char -> Alias (T_Raw { c = "Char"; is_primitive = true })
      | Types.T_Box boxed ->
        let boxed = transpile_ty boxed in
        let macro_arg = ty_to_string boxed in
        (match ctx.runtime_defined_list_types |> StringMap.find_opt macro_arg with
         | Some name -> Alias (Named name)
         | None ->
           let name = "Box_" ^ macro_arg in
           ctx.types
           <- ctx.types
              |> StringMap.add
                   name
                   ({ shape =
                        DEF_Raw
                          { def = make_string "define_Box(%s)" macro_arg
                          ; need_declared = [ Named macro_arg ]
                          ; need_completed = []
                          }
                    ; comment = None
                    }
                    : C_ast.ty_def);
           let impl_name = name ^ "_impl" in
           ctx.types
           <- ctx.types
              |> StringMap.add
                   impl_name
                   ({ shape =
                        DEF_Raw
                          { def = make_string "impl_Box(%s)" macro_arg
                          ; need_declared = []
                          ; need_completed = [ Named macro_arg ]
                          }
                    ; comment = None
                    }
                    : C_ast.ty_def);
           Alias (Named name))
      | Types.T_Ref { mut = _; referenced } -> Alias (Ptr (transpile_ty referenced))
      | Types.T_Variant ty ->
        if variant_needs_tag ty
        then
          Struct
            (StringMap.of_list
               [ "tag", variant_tag_ty ty_name ty
               ; "data", variant_data_ty_impl ty_name ty
               ])
        else Struct (StringMap.singleton "data" (variant_data_ty_impl ty_name ty))
      | Types.T_Tuple { name; tuple } ->
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
        if !boxed_structs
        then (
          let struct_name =
            gen_name
              (make_string "%t_DEF" (fun fmt ->
                 Print.print_optionally_named
                   ~always_print_shape:false
                   fmt
                   name
                   (fun fmt -> fprintf fmt "anonymous_tuple")))
          in
          ctx.types
          <- ctx.types
             |> StringMap.add
                  struct_name
                  ({ shape = C_ast.Struct fields
                   ; comment = Some (make_string "%a" Print.print_ty_shape ty)
                   }
                   : C_ast.ty_def);
          Alias (Ptr (Named struct_name)))
        else Struct fields
      | Types.T_List { element_ty } ->
        let element_ty = transpile_ty element_ty in
        let macro_arg = ty_to_string element_ty in
        (match ctx.runtime_defined_list_types |> StringMap.find_opt macro_arg with
         | Some name -> Alias (Named name)
         | None ->
           let name = "ArrayList_" ^ macro_arg in
           ctx.types
           <- ctx.types
              |> StringMap.add
                   name
                   ({ shape =
                        DEF_Raw
                          { def = make_string "define_ArrayList(%s)" macro_arg
                          ; need_declared = [ Named macro_arg ]
                          ; need_completed = []
                          }
                    ; comment = None
                    }
                    : C_ast.ty_def);
           let impl_name = name ^ "_impl" in
           ctx.types
           <- ctx.types
              |> StringMap.add
                   impl_name
                   ({ shape =
                        DEF_Raw
                          { def = make_string "impl_ArrayList(%s)" macro_arg
                          ; need_declared = []
                          ; need_completed = [ Named macro_arg ]
                          }
                    ; comment = None
                    }
                    : C_ast.ty_def);
           Alias (Named name))
      | Types.T_Ty -> Alias (Ptr (Named "TypeInfo"))
      | Types.T_Fn { is_closure; call_convention; args; result } ->
        let is_closure = is_closure |> Inference.await_inferred_simple in
        let call_convention = call_convention |> Inference.await_inferred_simple in
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
        let result_ty = transpile_ty result in
        let result_ty : C_ast.ty =
          match result_ty with
          | T_Unit -> Void
          | other -> other
        in
        let define_macro_args =
          ty_to_string result_ty :: (args |> List.map ty_to_string)
        in
        let args = if is_closure then [ C_ast.Ptr Void ] @ args else args in
        let args =
          match call_convention with
          | None -> [ C_ast.Ptr (T_Raw { c = "Context"; is_primitive = false }) ] @ args
          | Some "C" -> args
          | _ -> fail "unknown call convention"
        in
        (match is_closure with
         | true ->
           Log.trace (fun log ->
             log "Looking for %a" (List.print String.print) define_macro_args);
           (match
              ctx.runtime_defined_closure_types
              |> StringListMap.find_opt define_macro_args
            with
            | Some ty -> Alias (Named ty)
            | None ->
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
              Struct
                (StringMap.of_list [ "captured", C_ast.Ptr Void; "f", Named raw_fn_ty ]))
         | false -> Fn { args; result_ty })
      | Types.T_Generic _ when true -> Alias T_Unit
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
      | Types.T_Ast -> Alias T_Unit
      | Types.T_UnwindToken _ -> Alias (Ptr (ty_shape_repr ty))
      | Types.T_Target -> failwith __LOC__
      | Types.T_ContextTy ->
        (* TODO maybe? *)
        Alias T_Unit
      | Types.T_ImplicitContext -> Alias (Named "Context")
      | Types.T_CompilerScope -> Alias T_Unit
      | Types.T_Opaque { name = _; native_name } ->
        (match native_name with
         | Some native_name -> Alias (T_Raw { c = native_name; is_primitive = false })
         | None ->
           fail
             "native name must be set for opaque types when transpiling to C: %a"
             Ty.Shape.print
             ty)
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
          does_match
            field
            (Field
               { obj =
                   (if !boxed_structs
                    then Deref (Claim pure_place_expr)
                    else pure_place_expr)
               ; field = member_name member
               })
        | Unpack pattern ->
          (match pattern.shape with
           | P_Placeholder -> ()
           | P_Binding _ -> ()
           | _ -> failwith __LOC__);
          had_unpack := true;
          Literal (Bool true))
      |> List.fold_left (fun a b : C_ast.expr -> And (a, b)) (Literal (Bool true))
    | Types.P_Variant { label_span = _; label; value } ->
      let tag_matches : C_ast.expr =
        Equal
          ( Claim (Field { obj = pure_place_expr; field = "tag" })
          , Claim (Ident (variant_tag_name pattern.data.signature.ty label)) )
      in
      (match value with
       | Some value ->
         let var = gen_name "matches" in
         let_var
           ~gc:false
           (T_Raw { c = "bool"; is_primitive = true })
           var
           tag_matches
           ~drop:None;
         insert_stmt
           (If
              { cond = Claim (Ident var)
              ; then_case =
                  new_block (fun () ->
                    insert_stmt
                      (Assign
                         { assignee = Ident var
                         ; value =
                             does_match
                               value
                               (Field
                                  { obj = Field { obj = pure_place_expr; field = "data" }
                                  ; field = make_correct_ident (Label.get_name label)
                                  })
                         }))
              ; else_case = None
              });
         Claim (Ident var)
       | None -> tag_matches)
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
        | Types.Claim -> claim_c pure_place_expr binding.ty
        | Types.ByRef { mut = _ } -> AddrOf pure_place_expr
      in
      declare_binding binding value
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
            (Field
               { obj =
                   (if !boxed_structs
                    then Deref (Claim pure_place_expr)
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
           (Field
              { obj = Field { obj = pure_place_expr; field = "data" }
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
              Struct
                (captures
                 |> List.map (fun (binding : binding) ->
                   ( binding_name binding
                   , let ty = transpile_ty binding.ty in
                     if def.is_move then ty else C_ast.Ptr ty ))
                 |> StringMap.of_list)
          ; comment = Some (make_string "captured of closure at %a" print_span def_span)
          }
        in
        ctx.types <- ctx.types |> StringMap.add name captured_ty_def;
        ( Some name
        , captures
          |> List.map (fun (binding : binding) : (Id.t * C_ast.place_expr) ->
            ( binding.id
            , let field : C_ast.place_expr =
                Field
                  { obj = Deref (Claim (Ident typed_captured_arg_name))
                  ; field = binding_name binding
                  }
              in
              if def.is_move then field else Deref (Claim field) ))
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
        | T_Unit -> Void
        | other -> other
      in
      let unwind_ctx : unwind_ctx =
        { insert_unwind =
            (fun () ->
              match result_ty with
              | Void -> insert_stmt ReturnVoid
              | _ -> insert_stmt (Return (uninitialized result_ty)))
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
            [ ({ name = ctx_var; ty = Ptr (Named "Context") } : C_ast.fn_arg) ] @ args
          | Some "C" -> args
          | Some other -> fail "unknown call convension %S" other
        in
        let body : C_ast.block =
          let result_var = ref None in
          let block =
            new_block (fun () ->
              let scope : scope = { ctx_place = Deref (Claim (Ident ctx_var)) } in
              try
                captured_bindings
                |> List.iter (fun (binding : binding) ->
                  insert_stmt (Comment (make_string "captured %a" Binding.print binding)));
                (match captured_ty_name with
                 | Some captured_ty_name ->
                   let_var
                     ~gc:false
                     (Ptr (Named captured_ty_name))
                     typed_captured_arg_name
                     (Claim (Ident captured_arg_name))
                     ~drop:(* TODO *) None
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
                  | Unpack packed ->
                    let packed_ty =
                      packed.data.signature.ty
                      |> Ty.await_inferred
                      |> Ty.Shape.expect_tuple
                      |> Option.unwrap
                    in
                    let var = gen_name "packed" in
                    let var_ty = transpile_ty packed.data.signature.ty in
                    declare_var
                      ~gc:false
                      var_ty
                      var
                      ~drop:(Some (generate_drop packed.data.signature.ty));
                    if !boxed_structs then malloc_typed_ptr ~boxed:true var_ty (Ident var);
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
                        (Assign
                           { assignee =
                               Field
                                 { obj = tuple_place var
                                 ; field = member_name packed_member
                                 }
                           ; value = Claim (Ident (member_name member))
                           }));
                    pattern_match packed (Ident var));
                unwind_ctx.cleanup_scope_without_unwind ();
                match eval_expr def.body with
                | None -> ()
                | Some result ->
                  (match result_ty with
                   | T_Unit | Void -> insert_stmt (Expr result)
                   | _ ->
                     let var = gen_name "fn_result" in
                     result_var := Some var;
                     let_var ~gc:false result_ty var result ~drop:None)
              with
              | effect GetScope, k -> Effect.continue k scope)
          in
          block
          @ (!result_var
             |> Option.map (fun result_var -> C_ast.Return (Claim (Ident result_var)))
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

  and transpile_value (value : value) : C_ast.place_expr =
    let ctx = Effect.perform GetCtx in
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    Inference.Var.setup_default_if_needed value.var;
    let value =
      Interpreter.Substitute_bindings.sub_value
        ~span
        ~state:(Interpreter.sub_here interpreter)
        value
    in
    Value.await_inferred value |> ignore;
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
                                 ; value = E_Native { parts = [ Raw "NULL" ] }
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
             insert_stmt
               (S_Native
                  { parts =
                      [ Raw
                          ("#ifdef USE_GC\n"
                           ^ "GC_add_roots(&"
                           ^ value_name
                           ^ ", (&"
                           ^ value_name
                           ^ ") + 1);\n"
                           ^ "#endif\n")
                      ]
                  });
             Dynarray.add_last ctx.statics static);
         Ident value_name
       with
       | effect GetCurrentBlock, k -> Effect.continue k ctx.init_statics)

  and transpile_value_shape (shape : Types.value_shape) : C_ast.expr =
    match shape with
    | V_Unit -> Unit
    | V_Bool x -> Literal (Bool x)
    | V_Int32 x | V_UInt32 x -> Literal (Int32 x)
    | V_Int64 x | V_UInt64 x -> Literal (Int64 x)
    | V_Float64 x | V_Float32 x -> Literal (Float64 x)
    | V_Char c -> Literal (Int32 (Int32.of_int (Uchar.to_int c)))
    | V_Box _ -> failwith __LOC__
    | V_Ref _ -> failwith __LOC__
    | V_String s ->
      Apply
        { f = E_Native { parts = [ Raw "String_from_C_StringView" ] }
        ; args = [ Literal (String s) ]
        }
    | V_StringView s ->
      Apply
        { f = E_Native { parts = [ Raw "StringView_from_C_StringView" ] }
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
      compound_literal ~kast:true (transpile_ty (Value.Shape.ty_of shape)) fields
    | V_List { ty = { element_ty }; elements } ->
      Block
        (new_block (fun () ->
           let var = gen_name "list" in
           let kast_ty = Value.Shape.ty_of shape in
           let ty = transpile_ty kast_ty in
           let_var
             ~gc:false
             ty
             var
             (E_Native
                { parts =
                    [ Raw (ty_to_string ty)
                    ; Raw "_new(&"
                    ; Raw (type_info_name_for (transpile_ty element_ty))
                    ; Raw ")"
                    ]
                })
             ~drop:None;
           elements
           |> Dynarray.iter (fun element ->
             insert_stmt
               (S_Native
                  { parts =
                      [ Raw (ty_to_string ty)
                      ; Raw "_push_back("
                      ; Interpolated (AddrOf (Ident var))
                      ; Raw ", "
                      ; Interpolated (Claim (transpile_place element))
                      ; Raw ")"
                      ]
                  }));
           insert_stmt (Expr (Claim (Ident var)))))
    | V_Variant { label; data; ty = _ } ->
      Block
        (new_block (fun () ->
           let ty = Value.Shape.ty_of shape in
           let var = gen_name "variant" in
           insert_stmt (DeclareVar { name = var; ty = transpile_ty ty });
           insert_stmt
             (Assign
                { assignee = Field { obj = Ident var; field = "tag" }
                ; value = Claim (Ident (variant_tag_name ty label))
                });
           (match data with
            | None -> ()
            | Some data ->
              insert_stmt
                (Assign
                   { assignee =
                       Field
                         { obj = Field { obj = Ident var; field = "data" }
                         ; field = make_correct_ident (Label.get_name label)
                         }
                   ; value = Claim (transpile_place data)
                   }));
           insert_stmt (Expr (Claim (Ident var)))))
    | V_Ty ty ->
      E_Native { parts = [ Raw "&"; Raw (type_info_name_for (transpile_ty ty)) ] }
    | V_Fn _ | V_Generic _ -> fail "unreachable, we should never compile fns as consts"
    | V_NativeFn f -> fail "transpiling native fn %S at %s" f.name __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ ->
      (* TODO maybe? *)
      Unit
    | V_ImplicitContext _ -> failwith __LOC__
    | V_CompilerScope _ -> Unit
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
            (Field
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
          declare_var
            ~gc:false
            var_ty
            var
            ~drop:(Some (generate_drop packed.data.signature.ty));
          if !boxed_structs then malloc_typed_ptr ~boxed:true var_ty (Ident var);
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
              (Assign
                 { assignee = Field { obj = tuple_place var; field = member_name member }
                 ; value =
                     Claim
                       (Field
                          { obj = Deref (Claim pure_place_expr)
                          ; field = member_name original_member
                          })
                 }));
          assign packed (Ident var))
    | Types.A_Place place -> assign_to_place place pure_place_expr
    | Types.A_Let pattern -> pattern_match pattern pure_place_expr
    | Types.A_Error -> failwith __LOC__

  and assign_to_place (place : Expr.Place.t) (value : C_ast.place_expr) =
    let ty = place.data.signature.ty in
    let place = transpile_place_expr place in
    insert_stmt
      (Expr (Apply { f = Claim (Ident (generate_drop ty)); args = [ Claim place ] }));
    insert_stmt (Assign { assignee = place; value = claim_c value ty })

  and call_fn ~(args_is_tuple : bool) (f_expr : expr) (arg : expr) : C_ast.expr option =
    let f_ty =
      match f_expr.data.signature.ty |> Ty.await_inferred with
      | T_Fn ty -> ty
      | T_Generic _ -> failwith __LOC__
      | _ -> fail "Expected fn, got %a" Ty.print f_expr.data.signature.ty
    in
    let f = transpile_expr f_expr in
    let args =
      if args_is_tuple
      then (
        let args : C_ast.expr tuple ref = ref Tuple.empty in
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
                     (C_ast.Claim
                        (transpile_value (Interpreter.read_place ~span field.place))))
         | E_Tuple { parts; _ } ->
           parts
           |> List.iter (function
             | (Field { label; field : expr; _ } : _ Types.tuple_part_of) ->
               let name = label |> Option.map Label.get_name in
               let var = gen_name "arg" in
               let_var
                 ~gc:false
                 (transpile_ty field.data.signature.ty)
                 var
                 (transpile_expr field)
                 ~drop:(Some (generate_drop field.data.signature.ty));
               args
               := !args |> Tuple.add name (claim_c (Ident var) field.data.signature.ty)
             | Unpack packed ->
               let packed_ty =
                 packed.data.signature.ty
                 |> Ty.await_inferred
                 |> Ty.Shape.expect_tuple
                 |> Option.unwrap
               in
               let var = gen_name "packed" in
               let_var
                 ~gc:false
                 (transpile_ty packed.data.signature.ty)
                 var
                 (transpile_expr packed)
                 ~drop:(Some (generate_drop packed.data.signature.ty));
               packed_ty.tuple
               |> Tuple.iter (fun member (field : Types.ty_tuple_field) ->
                 args
                 := !args
                    |> Tuple.add
                         (match member with
                          | Index _ -> None
                          | Name name -> Some name)
                         (claim_c
                            (Field { obj = tuple_place var; field = member_name member })
                            field.ty)))
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
                !args |> Tuple.get member)
        |> List.of_seq)
      else [ transpile_expr arg ]
    in
    let f, args =
      if f_ty.is_closure |> Inference.await_inferred_simple
      then (
        let f_name = gen_name "f" in
        let_var ~gc:false (transpile_ty f_expr.data.signature.ty) f_name f ~drop:None;
        ( C_ast.Claim (Field { obj = Ident f_name; field = "f" })
        , [ C_ast.Claim (Field { obj = Ident f_name; field = "captured" }) ] @ args ))
      else f, args
    in
    let args =
      match f_ty.call_convention |> Inference.await_inferred_simple with
      | None -> [ C_ast.AddrOf (Effect.perform GetScope).ctx_place ] @ args
      | Some "C" -> args
      | Some other -> fail "unknown conv %S" other
    in
    let result_var = gen_name "apply_result" in
    let result_ty = transpile_ty f_ty.result in
    let apply_expr : C_ast.expr = Apply { f; args } in
    (match result_ty with
     | T_Unit | Void -> insert_stmt (Expr apply_expr)
     | _ ->
       let_var
         ~gc:false
         result_ty
         result_var
         apply_expr
         ~drop:(Some (generate_drop f_ty.result)));
    insert_stmt
      (If
         { cond = E_Native { parts = [ Raw "are_we_unwinding()" ] }
         ; then_case =
             new_block (fun () -> (Effect.perform GetUnwindCtx).insert_unwind ())
         ; else_case = None
         });
    match result_ty with
    | T_Unit | Void -> None
    | _ -> Some (claim_c (Ident result_var) f_ty.result)

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
    Field { obj = scope.ctx_place; field = context_field_name context_ty }

  and claim (place : Types.place_expr) : C_ast.expr =
    let c_place = transpile_place_expr place in
    claim_c c_place place.data.signature.ty

  and claim_c (c_place : C_ast.place_expr) (ty : ty) : C_ast.expr =
    Apply { f = Claim (Ident (generate_claim ty)); args = [ AddrOf c_place ] }

  and execute_expr (expr : expr) : unit =
    match eval_expr expr with
    | None -> ()
    | Some e -> insert_drop e expr.data.signature.ty

  and eval_expr (expr : expr) : C_ast.expr option =
    Log.trace (fun log ->
      log "transpiling %a at %a" Print.print_expr_short expr Span.print expr.data.span);
    let ctx = Effect.perform GetCtx in
    let interpreter = (Effect.perform CurrentFnCaptured).interpreter_state in
    let span = expr.data.span in
    Inference.Var.setup_default_if_needed expr.data.signature.ty.var;
    try
      match expr.shape with
      | Types.E_Constant { id = _; value } -> Some (Claim (transpile_value value))
      | Types.E_Ref { mut = _; place } -> Some (AddrOf (transpile_place_expr place))
      | Types.E_Claim place -> Some (claim place)
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
        with_new_scope (fun () ->
          match eval_expr expr with
          | Some result ->
            let var = gen_name "scope_result" in
            let_var ~gc:false (transpile_ty expr.data.signature.ty) var result ~drop:None;
            Some (C_ast.Claim (Ident var))
          | None -> None)
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
            (DeclareVar { name = result_var; ty = transpile_ty expr.data.signature.ty });
          let captured_arg : C_ast.expr =
            match transpiled_fn.captured_ty_name with
            | None -> E_Native { parts = [ Raw "NULL" ] }
            | Some captured_ty_name ->
              let captured_var_name = gen_name "captured" in
              let captured_place : C_ast.place_expr =
                Deref (Claim (Ident captured_var_name))
              in
              let gc = true in
              declare_var ~gc (Named captured_ty_name) captured_var_name ~drop:None;
              transpiled_fn.captured
              |> Id.Map.iter (fun _id (binding : binding) ->
                insert_stmt
                  (Assign
                     { assignee =
                         Field { obj = captured_place; field = binding_name binding }
                     ; value =
                         (if transpiled_fn.is_move
                          then claim_c (lookup_binding binding) binding.ty
                          else AddrOf (lookup_binding binding))
                     }));
              if gc
              then Claim (Ident captured_var_name)
              else AddrOf (Ident captured_var_name)
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
            transpile_fn ~call_convention ~is_closure ~captured:None def
          in
          Some (Claim (Ident transpiled_fn.name)))
      | Types.E_Generic { def; _ } ->
        (* TODO memoization? *)
        failwith __LOC__
        (* Fn (transpile_fn ~captured:None def) *)
      | Types.E_Tuple { guaranteed_anonymous = _; parts } ->
        let var_name = gen_name "tuple" in
        let var_ty = transpile_ty expr.data.signature.ty in
        insert_stmt (DeclareVar { name = var_name; ty = var_ty });
        if !boxed_structs then malloc_typed_ptr ~boxed:true var_ty (Ident var_name);
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
                 { assignee = Field { obj = tuple_place var_name; field = field_name }
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
              ~gc:false
              (transpile_ty packed.data.signature.ty)
              packed_name
              (transpile_expr packed)
              ~drop:None;
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
                       Field
                         { obj = tuple_place var_name
                         ; field = member_name assignee_member
                         }
                   ; value =
                       Claim
                         (Field
                            { obj = tuple_place packed_name; field = member_name member })
                   })));
        Some (Claim (Ident var_name))
      | Types.E_Variant { label; value; _ } ->
        let value =
          match value with
          | Some value -> transpile_expr value
          | None -> Unit
        in
        Some
          (let var = gen_name "variant" in
           insert_stmt
             (DeclareVar { name = var; ty = transpile_ty expr.data.signature.ty });
           insert_stmt
             (Assign
                { assignee = Field { obj = Ident var; field = "tag" }
                ; value = Claim (Ident (variant_tag_name expr.data.signature.ty label))
                });
           insert_stmt
             (Assign
                { assignee =
                    Field
                      { obj = Field { obj = Ident var; field = "data" }
                      ; field = make_correct_ident (Label.get_name label)
                      }
                ; value
                });
           Claim (Ident var))
      | Types.E_Apply { f; arg } -> call_fn ~args_is_tuple:true f arg
      | Types.E_InstantiateGeneric _ ->
        let value = Interpreter.eval interpreter expr in
        Some (Claim (transpile_value value))
      | Types.E_Assign { assignee; value } ->
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
              | TyExpr e ->
                Raw (Interpreter.eval_ty interpreter e |> transpile_ty |> ty_to_string)
              | Expr e -> Interpolated (transpile_expr e))
          in
          Some (E_Native { parts }))
      | Types.E_Module { def; bindings } ->
        let var = gen_name "module" in
        let binding_module_map = ref (Effect.perform GetBindingModuleMap) in
        bindings
        |> List.iter (fun (binding : binding) ->
          binding_module_map := !binding_module_map |> Id.Map.add binding.id var);
        let binding_module_map = !binding_module_map in
        (try
           let var_ty = transpile_ty expr.data.signature.ty in
           declare_var ~gc:(is_full_gc ()) var_ty var ~drop:None;
           let module_place : C_ast.place_expr = ident_place var in
           if !boxed_structs then malloc_typed_ptr ~boxed:true var_ty module_place;
           execute_expr def;
           Some (Claim module_place)
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
      | Types.E_And { lhs; rhs } ->
        let result = gen_name "and_result" in
        let_var
          ~gc:false
          (T_Raw { c = "Bool"; is_primitive = true })
          result
          (transpile_expr lhs)
          ~drop:None;
        insert_stmt
          (If
             { cond = Claim (Ident result)
             ; then_case =
                 new_block (fun () ->
                   insert_stmt
                     (Assign { assignee = Ident result; value = transpile_expr rhs }))
             ; else_case = None
             });
        Some (Claim (Ident result))
      | Types.E_Or { lhs; rhs } ->
        let result = gen_name "or_result" in
        let_var
          ~gc:false
          (T_Raw { c = "Bool"; is_primitive = true })
          result
          (transpile_expr lhs)
          ~drop:None;
        insert_stmt
          (If
             { cond = Not (Claim (Ident result))
             ; then_case =
                 new_block (fun () ->
                   insert_stmt
                     (Assign { assignee = Ident result; value = transpile_expr rhs }))
             ; else_case = None
             });
        Some (Claim (Ident result))
      | Types.E_Match { value; branches } ->
        let value_var = gen_name "matched" in
        (* TODO panic(not exhaustive) *)
        let_var
          ~gc:false
          (Ptr (transpile_ty value.data.signature.ty))
          value_var
          (AddrOf (transpile_place_expr value))
          ~drop:None;
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
        insert_stmt (S_Native { parts = [ Raw "Kast_match_non_exhaustive()" ] });
        insert_stmt (GotoLabel end_of_match_label);
        Some (Claim (Ident result_var))
      | Types.E_QuoteAst _ -> failwith __LOC__
      | Types.E_Loop { body } ->
        insert_stmt (For { body = new_block (fun () -> execute_expr body) });
        None
      | Types.E_Unwindable { token; body } ->
        let label_on_unwind = gen_name "on_unwind" in
        let label_result = gen_name "unwindable_result" in
        let token_var = gen_name "unwindable_token" in
        let token_ty = ty_repr token.data.signature.ty in
        let_var
          ~gc:false
          token_ty
          token_var
          (compound_literal
             ~kast:true
             token_ty
             [ "raw", C_ast.E_Native { parts = [ Raw "RawUnwindToken_new()" ] } ])
          ~drop:None;
        let token_ref_var = gen_name "unwindable_token_ref" in
        let_var
          ~gc:false
          (Ptr token_ty)
          token_ref_var
          (AddrOf (Ident token_var))
          ~drop:None;
        pattern_match token (Ident token_ref_var);
        let result_place : C_ast.place_expr =
          Field { obj = tuple_place token_var; field = "value" }
        in
        let old_unwind_ctx = Effect.perform GetUnwindCtx in
        let unwind_ctx : unwind_ctx =
          { insert_unwind = (fun () -> insert_stmt (Goto { label = label_on_unwind }))
          ; cleanup_scope_without_unwind = (fun () -> ())
          }
        in
        (try
           match eval_expr body with
           | None -> ()
           | Some result ->
             insert_stmt (Assign { assignee = result_place; value = result })
         with
         | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx);
        insert_stmt (Goto { label = label_result });
        insert_stmt (GotoLabel label_on_unwind);
        insert_stmt
          (If
             { cond =
                 Apply
                   { f = E_Native { parts = [ Raw "are_we_unwinding_with" ] }
                   ; args =
                       [ Claim (Field { obj = tuple_place token_var; field = "raw" }) ]
                   }
             ; then_case =
                 new_block (fun () ->
                   insert_stmt (S_Native { parts = [ Raw "stop_unwinding()" ] });
                   insert_stmt (Goto { label = label_result }))
             ; else_case = Some (new_block (fun () -> old_unwind_ctx.insert_unwind ()))
             });
        insert_stmt (GotoLabel label_result);
        Some (Claim result_place)
        (* let token_ident = gen_name "token" in *)
        (* Unwindable *)
        (*   { token_ident *)
        (*   ; body = Then [ pattern_match token (Ident token_ident); transpile_expr body ] *)
        (*   } *)
      | Types.E_Unwind { token; value } ->
        let token_var = gen_name "token" in
        let_var
          ~gc:false
          (transpile_ty token.data.signature.ty)
          token_var
          (transpile_expr token)
          ~drop:None;
        (* token->value = value *)
        insert_stmt
          (Assign
             { assignee =
                 Field { obj = Deref (Claim (tuple_place token_var)); field = "value" }
             ; value = transpile_expr value
             });
        (* currently_unwinding = token->raw *)
        insert_stmt
          (Expr
             (Apply
                { f = E_Native { parts = [ Raw "start_unwinding" ] }
                ; args =
                    [ Claim
                        (Field
                           { obj = Deref (Claim (tuple_place token_var)); field = "raw" })
                    ]
                }));
        (Effect.perform GetUnwindCtx).insert_unwind ();
        None
      | Types.E_InjectContext { context_ty; value } ->
        let value = transpile_expr value in
        let old_var = gen_name "old_ctx" in
        let ctx_place = current_context context_ty in
        let_var
          ~gc:false
          (transpile_ty context_ty.ty)
          old_var
          (Claim ctx_place)
          ~drop:None;
        insert_stmt (Assign { assignee = ctx_place; value });
        defer (fun () ->
          insert_stmt (Assign { assignee = ctx_place; value = Claim (Ident old_var) }));
        None
      | Types.E_LetRefContext new_ref ->
        let new_ctx_var = gen_name "ctx" in
        let_var
          ~gc:false
          (T_Raw { c = "Context*"; is_primitive = false })
          new_ctx_var
          (transpile_expr new_ref)
          ~drop:None;
        (Effect.perform GetScope).ctx_place <- Deref (Claim (Ident new_ctx_var));
        None
      | Types.E_ImplCast _ -> None
      | Types.E_Cast _ ->
        let value = Interpreter.eval interpreter expr in
        Some (Claim (transpile_value value))
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

  let postprocess (program : C_ast.program) : C_ast.program =
    let statics =
      program.statics
      |> List.map (fun (static : C_ast.static) -> static.name)
      |> StringSet.of_list
    in
    let postprocess_fn (name : string) (fn : C_ast.fn_def) : C_ast.fn_def =
      let new_body =
        match !gc_mode with
        | EscapeAnalyze ->
          let escaping_idents = ref StringSet.empty in
          let analyzed = ref false in
          let rec walk_block (block : C_ast.block) : C_ast.block =
            new_block (fun () ->
              block |> List.iter (fun stmt -> insert_stmt (walk_stmt stmt)))
          and walk_native_expr ({ parts } : C_ast.native_expr) : C_ast.native_expr =
            { parts =
                parts
                |> List.map
                     (fun (part : C_ast.native_expr_part) : C_ast.native_expr_part ->
                        match part with
                        | Interpolated e -> Interpolated (walk_expr e)
                        | Raw s -> Raw s)
            }
          and walk_place_expr ~(escaping : bool) (place : C_ast.place_expr)
            : C_ast.place_expr
            =
            match place with
            | C_ast.Ident ident ->
              if !analyzed
              then (
                let ident_does_escape = !escaping_idents |> StringSet.contains ident in
                if ident_does_escape then Deref (Claim (Ident ident)) else Ident ident)
              else (
                if escaping && not (statics |> StringSet.contains ident)
                then escaping_idents := !escaping_idents |> StringSet.add ident;
                Ident ident)
            | C_ast.P_Native e -> P_Native (walk_native_expr e)
            | C_ast.Field { obj; field : string } ->
              Field { obj = walk_place_expr ~escaping obj; field }
            | C_ast.Deref ptr -> Deref (walk_expr ptr)
            | C_ast.Temp e -> Temp (walk_expr e)
          and walk_expr (expr : C_ast.expr) : C_ast.expr =
            match expr with
            | Unit -> Unit
            | Literal literal -> Literal literal
            | E_Native e -> E_Native (walk_native_expr e)
            | Claim place -> Claim (walk_place_expr ~escaping:false place)
            | Cast { value; target : C_ast.ty } ->
              Cast { value = walk_expr value; target }
            | AddrOf place -> AddrOf (walk_place_expr ~escaping:true place)
            | Not e -> Not (walk_expr e)
            | And (a, b) -> And (walk_expr a, walk_expr b)
            | C_ast.Or (a, b) -> Or (walk_expr a, walk_expr b)
            | C_ast.Equal (a, b) -> Equal (walk_expr a, walk_expr b)
            | C_ast.Apply { f; args } ->
              Apply { f = walk_expr f; args = args |> List.map walk_expr }
            | C_ast.Block b -> Block (walk_block b)
          and walk_stmt (stmt : C_ast.stmt) : C_ast.stmt =
            match stmt with
            | S_Native e -> S_Native (walk_native_expr e)
            | Comment c -> Comment c
            | DeclareVar { name; ty } ->
              if !analyzed
              then
                if !escaping_idents |> StringSet.contains name
                then (
                  insert_stmt (DeclareVar { name; ty = Ptr ty });
                  malloc_typed_ptr ~boxed:false ty (Ident name);
                  Comment (make_string "%s is potentially escaping, so its gc'd" name))
                else DeclareVar { name; ty }
              else DeclareVar { name; ty }
            | Expr e -> Expr (walk_expr e)
            | If { cond; then_case; else_case } ->
              let cond = walk_expr cond in
              let then_case = walk_block then_case in
              let else_case = else_case |> Option.map walk_block in
              If { cond; then_case; else_case }
            | Switch { value; cases; default } ->
              let value = walk_expr value in
              let cases =
                cases
                |> List.map
                     (fun ({ value; body } : C_ast.switch_case) : C_ast.switch_case ->
                        let value = walk_expr value in
                        let body = walk_block body in
                        { value; body })
              in
              let default = default |> Option.map walk_block in
              Switch { value; cases; default }
            | Assign { assignee; value } ->
              Assign
                { assignee = walk_place_expr ~escaping:false assignee
                ; value = walk_expr value
                }
            | Goto { label } -> Goto { label }
            | GotoLabel label -> GotoLabel label
            | For { body } -> For { body = walk_block body }
            | Return e -> Return (walk_expr e)
            | ReturnVoid -> ReturnVoid
          in
          let _ = walk_block fn.body in
          Log.trace (fun log ->
            log
              "escaped from %S: %a"
              name
              (List.print String.print)
              (StringSet.to_list !escaping_idents));
          analyzed := true;
          walk_block fn.body
        | _ -> fn.body
      in
      { fn with body = new_body }
    in
    let fns = program.fns |> StringMap.mapi postprocess_fn in
    let ctx = Effect.perform GetCtx in
    let type_info_statics =
      ctx.type_infos
      |> CTyMap.to_list
      |> List.map (fun ((ty, name) : C_ast.ty * string) : C_ast.static ->
        { ty = T_Raw { c = "TypeInfo"; is_primitive = false }; name; comment = None })
    in
    let rec construct_ty_def_type_info (name : string) (def : C_ast.ty_def) : C_ast.expr =
      match def.shape with
      | C_ast.Alias ty -> construct_ty_type_info ty
      | _ ->
        let kind, inner_ptrs, gc_descr =
          match def.shape with
          | C_ast.Struct fields | C_ast.Union fields ->
            let bitmap_var = gen_name "gc_bitmap" in
            (match !gc_mode with
             | Disabled -> ()
             | _ ->
               insert_stmt
                 (S_Native
                    { parts =
                        [ Raw "GC_word "
                        ; Raw bitmap_var
                        ; Raw "[GC_BITMAP_SIZE("
                        ; Raw name
                        ; Raw ")]"
                        ]
                    });
               insert_stmt
                 (S_Native
                    { parts =
                        [ Raw "memset(&"
                        ; Raw bitmap_var
                        ; Raw ", 0, sizeof("
                        ; Raw bitmap_var
                        ; Raw "))"
                        ]
                    }));
            let is_primitive = ref true in
            let inner_ptrs = ref 0 in
            let mark_field_as_ptr (full_name : string) =
              is_primitive := false;
              inner_ptrs := !inner_ptrs + 1;
              (* GC_set_bit(T_bitmap, GC_WORD_OFFSET(T,field)); *)
              match !gc_mode with
              | Disabled -> ()
              | _ ->
                insert_stmt
                  (S_Native
                     { parts =
                         [ Raw "GC_set_bit("
                         ; Raw bitmap_var
                         ; Raw ", GC_WORD_OFFSET("
                         ; Raw name
                         ; Raw ", "
                         ; Raw full_name
                         ; Raw "))"
                         ]
                     })
            in
            let mark_field_as_raw (full_name : string) (ty : C_ast.ty) =
              is_primitive := false;
              (* TODO actually more ptrs than 1 here *)
              inner_ptrs := !inner_ptrs + 1;
              let ty_as_string =
                let result = ref "" in
                try
                  C_ast.Print.print_ty ty;
                  !result
                with
                | effect C_ast.Print.GetOutput, k ->
                  Effect.continue k (fun s -> result := !result ^ s)
              in
              match !gc_mode with
              | Disabled -> ()
              | _ ->
                insert_stmt
                  (S_Native
                     { parts =
                         [ Raw "for (size_t i = 0; i < GC_WORD_LEN("
                         ; Raw ty_as_string
                         ; Raw "); i++) GC_set_bit("
                         ; Raw bitmap_var
                         ; Raw ", GC_WORD_OFFSET("
                         ; Raw name
                         ; Raw ", "
                         ; Raw full_name
                         ; Raw ") + i)"
                         ]
                     })
            in
            let rec walk_field (full_name : string) (ty : C_ast.ty) =
              match ty with
              | T_Unit -> ()
              | T_Raw { c = _; is_primitive } ->
                if not is_primitive then mark_field_as_raw full_name ty
              | Named name ->
                walk_field_def full_name name (ctx.types |> StringMap.find name)
              | Ptr _ -> mark_field_as_ptr full_name
              | Void -> fail "void field is impossible"
            and walk_field_def
                  (full_name : string)
                  (ty_name : string)
                  (def : C_ast.ty_def)
              =
              match def.shape with
              | C_ast.Enum _ -> ()
              | C_ast.Struct inner_fields | C_ast.Union inner_fields ->
                inner_fields
                |> StringMap.iter (fun inner_field inner_ty ->
                  walk_field (full_name ^ "." ^ inner_field) inner_ty)
              | C_ast.Fn _ -> mark_field_as_ptr full_name
              | C_ast.Alias ty -> walk_field full_name ty
              | DEF_Raw _ -> mark_field_as_raw full_name (Named ty_name)
              | RuntimeDefined _ ->
                fail "TODO plain C data inside Kast??? ty name=%S" ty_name
            in
            fields |> StringMap.iter walk_field;
            if false && !is_primitive
            then "primitive", 0, None
            else
              ( "object"
              , !inner_ptrs
              , Some
                  ( "gc_descr"
                  , C_ast.E_Native
                      { parts =
                          [ Raw "GC_make_descriptor("
                          ; Raw bitmap_var
                          ; Raw ", GC_WORD_LEN("
                          ; Raw name
                          ; Raw "))"
                          ]
                      } ) )
          | Enum _ -> "primitive", 0, None
          | RuntimeDefined -> "raw", 0, None
          | Fn _ -> "raw", 0, None
          | DEF_Raw _ -> "raw", 0, None
          | Alias _ -> failwith __LOC__
        in
        compound_literal
          ~kast:false
          (T_Raw { c = "TypeInfo"; is_primitive = false })
          ([ "name", C_ast.Literal (String name)
           ; "alignment", C_ast.E_Native { parts = [ Raw "alignof("; Raw name; Raw ")" ] }
           ; "size", E_Native { parts = [ Raw "sizeof("; Raw name; Raw ")" ] }
           ; "stride", E_Native { parts = [ Raw "sizeof("; Raw name; Raw ")" ] }
           ; "kind", E_Native { parts = [ Raw ("TypeInfoKind_" ^ kind) ] }
           ]
           @ (if !allocation_stats
              then
                [ ( "allocation_stats"
                  , C_ast.E_Native { parts = [ Raw "Kast_type_allocation_stats_new()" ] }
                  )
                ]
              else [])
           @
           match !gc_mode with
           | Disabled -> []
           | _ ->
             [ "gc_inner_ptrs", C_ast.Literal (Int32 (Int32.of_int inner_ptrs)) ]
             @ (gc_descr |> Option.to_list))
    and construct_ty_type_info (ty : C_ast.ty) =
      let primitive s : C_ast.expr =
        E_Native { parts = [ Raw "TypeInfo_simple(primitive, "; Raw s; Raw ")" ] }
      in
      match ty with
      | T_Unit -> primitive "Unit"
      | T_Raw { c = raw_ty; is_primitive } ->
        if raw_ty = "String"
        then E_Native { parts = [ Raw "String_TypeInfo" ] }
        else if is_primitive
        then
          E_Native { parts = [ Raw "TypeInfo_simple(primitive, "; Raw raw_ty; Raw ")" ] }
        else E_Native { parts = [ Raw "TypeInfo_simple(raw, "; Raw raw_ty; Raw ")" ] }
      | Named name ->
        let def =
          ctx.types
          |> StringMap.find_opt name
          |> Option.unwrap_or_else (fun () -> failwith __LOC__)
        in
        construct_ty_def_type_info name def
      | Ptr _ -> primitive "void*"
      | Void -> fail "tried to create type info for void?"
    in
    let init_type_infos_fn : C_ast.fn_def =
      { args = []
      ; result_ty = Void
      ; comment = None
      ; body =
          new_block (fun () ->
            ctx.type_infos
            |> CTyMap.iter (fun ty name ->
              insert_stmt
                (Assign { assignee = Ident name; value = construct_ty_type_info ty })))
      }
    in
    let fns = fns |> StringMap.add "Kast_init_user_type_infos" init_type_infos_fn in
    let statics = program.statics @ type_info_statics in
    { program with fns; statics }
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
    ; claim_fns = ValueMap.empty
    ; types =
        StringMap.of_list
          ([ "Unit"
           ; "Byte"
           ; "Bool"
           ; "Int32"
           ; "UInt32"
           ; "Float32"
           ; "Float64"
           ; "String"
           ; "Char"
           ; "Int64"
           ; "UInt64"
           ; "TypeInfo"
           ; "Context"
           ]
           |> List.map (fun name : (string * C_ast.ty_def) ->
             name, { shape = C_ast.RuntimeDefined; comment = None }))
    ; fns = StringMap.empty
    ; init_statics = { stmts = [] }
    ; includes = StringSet.empty
    ; contexts = Id.Map.empty
    ; type_infos = CTyMap.empty
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
        (fun () -> Impl.insert_stmt (Return (Literal (Int32 (Int32.of_int (-1))))))
    ; cleanup_scope_without_unwind = (fun () -> ())
    }
  in
  let scope : scope =
    { ctx_place =
        (match !gc_mode with
         | Full -> Deref (Claim (Ident ctx_var))
         | _ -> Ident ctx_var)
    }
  in
  try
    let main : C_ast.fn_def =
      { args =
          [ { name = "argc"; ty = T_Raw { c = "int"; is_primitive = true } }
          ; { name = "argv"; ty = T_Raw { c = "char**"; is_primitive = false } }
          ]
      ; result_ty = T_Raw { c = "int"; is_primitive = true }
      ; body =
          Impl.new_block (fun () ->
            Impl.insert_stmt (S_Native { parts = [ Raw "Kast_init(argc, argv)" ] });
            Impl.declare_var ~gc:(is_full_gc ()) (Named "Context") ctx_var ~drop:None;
            Impl.insert_stmt
              (Expr (Apply { f = Claim (Ident "KAST_init_statics"); args = [] }));
            Impl.execute_expr expr;
            unwind_ctx.cleanup_scope_without_unwind ())
          @ [ Return (Literal (Int32 (Int32.of_int 0))) ]
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
              Struct
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
               name, ({ shape = RuntimeDefined; comment = None } : C_ast.ty_def))
             |> StringMap.of_list)
       |> StringMap.union
            (fun _ a _ -> Some a)
            (ctx.runtime_defined_list_types
             |> StringMap.to_list
             |> List.map (fun (_, name) ->
               name, ({ shape = RuntimeDefined; comment = None } : C_ast.ty_def))
             |> StringMap.of_list);
    Impl.postprocess
      { types = ctx.types
      ; includes = ctx.includes
      ; fns = ctx.fns
      ; statics = ctx.statics |> Dynarray.to_list
      }
  with
  | effect GetUnwindCtx, k -> Effect.continue k unwind_ctx
  | effect GetScope, k -> Effect.continue k scope
  | effect CurrentFnCaptured, k -> Effect.continue k captured
  | effect GetCtx, k -> Effect.continue k ctx
  | effect GetBindingModuleMap, k -> Effect.continue k Id.Map.empty
;;
