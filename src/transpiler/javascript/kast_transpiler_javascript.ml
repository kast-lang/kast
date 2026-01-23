open Std
open Kast_util
open Kast_types
open Types
module JsAst = Javascript_ast
module Base64_vlq = Base64_vlq
module Writer = Writer

type no_effect_expr = NoEffect of JsAst.expr

let async_get_set = false
let async_fns = true

type transpiled_place =
  | OCaml of
      { get : unit -> no_effect_expr
      ; set : JsAst.expr -> unit
      }
  | Js of no_effect_expr

type mutable_ctx =
  { mutable captured_values : JsAst.name ValueMap.t
  ; mutable captured_places : transpiled_place Id.Map.t
  ; mutable symbols : JsAst.name Id.Map.t
  ; mutable prepend : JsAst.stmt list
  ; mutable prepend_late : JsAst.stmt list
  ; mutable module_of_binding : JsAst.name Id.Map.t
  }

type ctx =
  { interpreter : interpreter_state
  ; mut : mutable_ctx
  ; captured : interpreter_scope
  ; span : span
  ; target : value_target
  }

let ctx_var : JsAst.name = { raw = "ctx"; original = None }

type scope = { mutable stmts : JsAst.stmt list }
type _ Effect.t += GetScope : scope Effect.t

let execute_all stmts =
  let scope = Effect.perform GetScope in
  scope.stmts <- scope.stmts @ stmts
;;

let execute stmt = execute_all [ stmt ]

type _ Effect.t += GetCtx : ctx Effect.t

module Impl = struct
  let rec _unused () = ()
  and place_from_js (place : no_effect_expr) : transpiled_place = Js place
  and place_from_js_var name = place_from_js (NoEffect { shape = Var name; span = None })

  and place_of (expr : no_effect_expr) : transpiled_place =
    OCaml
      { get = (fun () -> expr)
      ; set =
          (fun value ->
            execute { shape = Assign { assignee = pure expr; value }; span = None })
      }

  and place_of_var name = place_of (NoEffect { shape = Var name; span = None })

  and place_of_var_field name field =
    place_of
      (NoEffect
         { shape = Field { obj = { shape = Var name; span = None }; field }; span = None })

  and place_to_js (place : transpiled_place) : no_effect_expr =
    match place with
    | Js place -> place
    | OCaml place ->
      NoEffect
        { shape =
            Obj
              [ Field
                  { name = "get"
                  ; value =
                      { shape =
                          JsAst.Fn
                            { async = async_get_set
                            ; args = []
                            ; body =
                                [ { shape =
                                      JsAst.Return
                                        (let (NoEffect get) = place.get () in
                                         get)
                                  ; span = None
                                  }
                                ]
                            }
                      ; span = None
                      }
                  }
              ; Field
                  { name = "set"
                  ; value =
                      (let var = JsAst.gen_name ~original:None "value" in
                       { shape =
                           JsAst.Fn
                             { async = async_get_set
                             ; args = [ var ]
                             ; body =
                                 scope (fun () ->
                                   place.set { shape = Var var; span = None })
                             }
                       ; span = None
                       })
                  }
              ]
        ; span = None
        }

  and not_inferred : 'a 'scope. ('a, 'scope) Inference.Var.t -> JsAst.expr =
    fun var ->
    Log.error (fun log -> log "transpiling not inferred var");
    { shape =
        JsAst.Obj
          [ Field
              { name = "type"; value = { shape = String "not inferred"; span = None } }
          ; Field
              { name = "spans"
              ; value =
                  { shape =
                      List
                        (Inference.Var.spans var
                         |> SpanSet.to_list
                         |> List.map (fun span : JsAst.expr ->
                           { shape = JsAst.String (make_string "%a" Span.print span)
                           ; span = None
                           }))
                  ; span = None
                  }
              }
          ]
    ; span = None
    }

  and todo f =
    Format.kdprintf
      (fun p ->
         Log.error (fun log -> log "%t" p);
         raise_error (make_string "%t" p))
      f

  and tuple_field_name : Tuple.member -> string = function
    | Name name -> name
    | Index i -> Int.to_string i

  and fn ~(captured : interpreter_scope option) (def : maybe_compiled_fn) : no_effect_expr
    =
    let ctx = Effect.perform GetCtx in
    let compiled = def |> Kast_interpreter.await_compiled ~span:ctx.span in
    let { arg; body } =
      compiled |> Option.unwrap_or_else (fun () -> fail "fn not compiled")
    in
    let ctx =
      match captured with
      | None -> ctx
      | Some captured -> { ctx with captured }
    in
    let arg_name = JsAst.gen_name ~original:None "arg" in
    try
      calculate
        { shape =
            JsAst.Fn
              { async = async_fns
              ; args = [ arg_name; ctx_var ]
              ; body =
                  scope (fun () ->
                    pattern_match arg (place_of_var arg_name);
                    let result = transpile_expr body in
                    execute { shape = JsAst.Return (pure result); span = None })
              }
        ; span = None
        }
    with
    | effect GetCtx, k -> Effect.continue k ctx

  and type_named : name_shape -> no_effect_expr =
    fun name ->
    let check_simple_part (part : name_part) =
      match part with
      | Uri _ -> true
      | Str _ -> true
      | Symbol _ -> true
    in
    let rec check_simple_name (name : name_shape) =
      match name with
      | Simple part -> check_simple_part part
      | Concat (a, b) -> check_simple_name a && check_simple_part b
      | Instantiation _ -> false
    in
    if check_simple_name name
    then (
      let name = make_string "%a" Print.print_name_shape name in
      let expr = make_string "Kast.types.create_or_find(%S)" name in
      calculate { shape = JsAst.Raw expr; span = None })
    else raise (Invalid_argument "only simple names pls")

  and transpile_ty : ty -> no_effect_expr =
    fun ty ->
    Inference.Var.setup_default_if_needed ty.var;
    match ty.var |> Inference.Var.inferred_opt with
    | None -> calculate <| not_inferred ty.var
    | Some shape -> shape |> transpile_ty_shape

  and transpile_ty_shape : ty_shape -> no_effect_expr =
    fun shape ->
    let todo_ty s : no_effect_expr =
      calculate { shape = JsAst.Raw (make_string "Kast.types.todo(%S)" s); span = None }
    in
    let primitive s : no_effect_expr =
      calculate
        { shape = JsAst.Raw (make_string "Kast.types.primitive[%S]" s); span = None }
    in
    match shape with
    | T_Unit -> primitive "Unit"
    | T_Bool -> primitive "Bool"
    | T_Int32 -> primitive "Int32"
    | T_Int64 -> primitive "Int64"
    | T_Float64 -> primitive "Float64"
    | T_String -> primitive "String"
    | T_Char -> primitive "Char"
    | T_Ref _ -> todo_ty __LOC__
    | T_Variant { name; variants = _ } ->
      (match name |> OptionalName.await_inferred with
       | Some name ->
         (try type_named name with
          | Invalid_argument _ -> todo_ty __LOC__)
       | None -> todo_ty __LOC__)
    | T_Tuple { name; tuple = _ } ->
      (match name |> OptionalName.await_inferred with
       | Some name ->
         (try type_named name with
          | Invalid_argument _ -> todo_ty __LOC__)
       | None -> todo_ty __LOC__)
    | T_Ty -> todo_ty __LOC__
    | T_Fn _ -> todo_ty __LOC__
    | T_Generic _ -> todo_ty __LOC__
    | T_Ast -> todo_ty __LOC__
    | T_UnwindToken _ -> todo_ty __LOC__
    | T_Target -> todo_ty __LOC__
    | T_ContextTy -> todo_ty __LOC__
    | T_CompilerScope -> todo_ty __LOC__
    | T_Opaque _ -> todo_ty __LOC__
    | T_Blocked _ -> todo_ty __LOC__
    | T_Error -> NoEffect { shape = JsAst.Null; span = None }

  and transpile_binding : span:span option -> binding -> transpiled_place =
    fun ~span binding ->
    let ctx = Effect.perform GetCtx in
    match ctx.mut.module_of_binding |> Id.Map.find_opt binding.id with
    | Some module_var -> place_of_var_field module_var binding.name.name
    | None ->
      let binding_span =
        match span with
        | Some span -> span
        | None -> binding.span
      in
      place_of_var (binding_name ~span:binding_span binding)

  and transpile_blocked : blocked_value -> transpiled_place =
    fun value ->
    let ctx = Effect.perform GetCtx in
    match value.shape with
    | BV_Binding binding -> transpile_binding ~span:None binding
    | BV_Instantiate _ -> failwith __LOC__
    | BV_ClaimRef _ -> failwith __LOC__
    | BV_FieldRef _ -> failwith __LOC__

  and with_global_scope f = prepend (scope f)

  and transpile_value : value -> no_effect_expr =
    fun value ->
    let ctx = Effect.perform GetCtx in
    Inference.Var.setup_default_if_needed value.var;
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked value) -> value |> transpile_blocked |> claim
    | _ ->
      let value_name = ref None in
      let do_prepend = ref false in
      ctx.mut.captured_values
      <- ctx.mut.captured_values
         |> ValueMap.update value (fun name ->
           let name =
             match name with
             | Some name -> name
             | None ->
               let name = JsAst.gen_name ~original:None "value" in
               do_prepend := true;
               name
           in
           value_name := Some name;
           Some name);
      let value_name = !value_name |> Option.get in
      if !do_prepend
      then (
        Log.trace (fun log -> log "prepend %a" Value.print value);
        with_global_scope (fun () ->
          execute
            { shape =
                JsAst.Let
                  { var = value_name
                  ; value =
                      (match value.var |> Inference.Var.inferred_opt with
                       | None -> not_inferred value.var
                       | Some value_shape -> value_shape |> transpile_value_shape |> pure)
                  }
            ; span = None
            }));
      (* TODO copy? *)
      NoEffect { shape = Var value_name; span = None }

  and transpile_value_shape : value_shape -> no_effect_expr =
    fun shape ->
    try
      let ctx = Effect.perform GetCtx in
      match shape with
      | V_Unit -> NoEffect { shape = JsAst.Null; span = None }
      | V_Bool x -> NoEffect { shape = JsAst.Bool x; span = None }
      | V_Int32 x -> NoEffect { shape = JsAst.Number (Int32.to_float x); span = None }
      | V_Int64 x -> NoEffect { shape = JsAst.Bigint (Int64.to_string x); span = None }
      | V_Float64 x -> NoEffect { shape = JsAst.Number x; span = None }
      | V_Char c -> NoEffect { shape = JsAst.String (String.make 1 c); span = None }
      | V_String s -> calculate { shape = JsAst.String s; span = None }
      | V_Ref _ -> failwith __LOC__
      | V_Tuple { tuple; ty = _ } ->
        calculate
          { shape =
              JsAst.Obj
                (tuple
                 |> Tuple.to_seq
                 |> Seq.map (fun (member, (field : value_tuple_field)) : JsAst.obj_part ->
                   let field_name = tuple_field_name member in
                   let value =
                     field.place |> Kast_interpreter.read_place ~span:ctx.span
                   in
                   Field { name = field_name; value = transpile_value value |> pure })
                 |> List.of_seq)
          ; span = None
          }
      | V_Variant { label; data; ty = _ } ->
        create_variant
          label
          (data |> Option.map (fun place -> place |> transpile_place |> read_place))
      | V_Ty ty -> transpile_ty ty
      | V_Fn { ty = _; fn = { def; captured; _ } } -> fn ~captured:(Some captured) def
      | V_Generic { name = _; fn = { def; captured; _ }; ty = _ } ->
        fn ~captured:(Some captured) def
      | V_NativeFn _ -> failwith __LOC__
      | V_Ast _ -> failwith __LOC__
      | V_UnwindToken _ -> failwith __LOC__
      | V_Target _ -> failwith __LOC__
      | V_ContextTy _ -> NoEffect { shape = JsAst.Null; span = None }
      | V_CompilerScope _ -> NoEffect { shape = JsAst.Undefined; span = None }
      | V_Opaque _ -> failwith __LOC__
      | V_Blocked _ -> failwith __LOC__
      | V_Error -> NoEffect { shape = JsAst.Undefined; span = None }
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling value shape %a" Value.Shape.print shape);
      raise e

  and binding_name : span:span -> binding -> JsAst.name =
    fun ~span binding ->
    { (* raw = make_string "%s_%d" binding.name.name binding.id.value; *)
      raw =
        (let name = binding.name.name in
         match name with
         | "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger"
         | "default" | "delete" | "do" | "else" | "export" | "extends" | "false"
         | "finally" | "for" | "function" | "if" | "import" | "in" | "instanceof" | "new"
         | "null" | "return" | "super" | "switch" | "this" | "throw" | "true" | "try"
         | "typeof" | "var" | "void" | "while" | "with" | _ ->
           make_string "%s_%d" name binding.id.value
         | _ -> name)
    ; original = Some { raw = binding.name.name; span }
    }

  and assign_to_place : transpiled_place -> no_effect_expr -> unit =
    fun place value ->
    match place with
    | OCaml place -> place.set (pure value)
    | Js place ->
      let (NoEffect place) = place in
      execute
        { shape =
            Expr
              { shape =
                  Call
                    { async = async_get_set
                    ; f = { shape = Field { obj = place; field = "set" }; span = None }
                    ; args = [ pure value ]
                    }
              ; span = None
              }
        ; span = None
        }

  and claim : transpiled_place -> no_effect_expr = fun place -> read_place place

  and read_place : transpiled_place -> no_effect_expr =
    fun place ->
    match place with
    | OCaml place -> place.get ()
    | Js place ->
      let (NoEffect place) = place in
      NoEffect
        { shape =
            Call
              { async = async_get_set
              ; f = { shape = Field { obj = place; field = "get" }; span = None }
              ; args = []
              }
        ; span = None
        }

  and does_match (pattern : pattern) (place : transpiled_place) : no_effect_expr =
    let span = Some pattern.data.span in
    match pattern.shape with
    | P_Placeholder -> NoEffect { shape = Bool true; span }
    | P_Ref referenced_pattern ->
      does_match referenced_pattern (place_from_js (read_place place))
    | P_Unit -> NoEffect { shape = Bool true; span }
    | P_Binding _ -> NoEffect { shape = Bool true; span }
    | P_Tuple { parts } ->
      let var = JsAst.gen_name ~original:None "matches" in
      let_var var (NoEffect { shape = Bool true; span = None });
      let matches_label = JsAst.gen_name ~original:None "matches" in
      let body =
        scope (fun () ->
          let index = ref 0 in
          parts
          |> List.iter (fun (part : pattern tuple_part_of) ->
            match part with
            | Field { label; label_span = _; field = field_pattern } ->
              let member =
                match label with
                | None ->
                  let member = Tuple.Member.Index !index in
                  index := !index + 1;
                  member
                | Some name -> Tuple.Member.Name (Label.get_name name)
              in
              execute
                { shape =
                    JsAst.If
                      { condition =
                          calculate
                            { shape =
                                Not
                                  (pure
                                   <| does_match field_pattern (field_place place member)
                                  )
                            ; span
                            }
                          |> pure
                      ; then_case =
                          scope (fun () ->
                            assign_var var (NoEffect { shape = Bool false; span = None });
                            execute { shape = LabelledBreak matches_label; span })
                      ; else_case = None
                      }
                ; span
                }
            | Unpack packed -> failwith __LOC__))
      in
      execute
        { shape =
            Labelled { label = matches_label; stmt = { shape = Block body; span = None } }
        ; span
        };
      NoEffect { shape = Var var; span = None }
    | P_Variant { label; label_span = _; value = value_pattern } ->
      let value = JsAst.gen_name ~original:None "variant" in
      execute
        { shape =
            Let
              { var = value
              ; value =
                  (let (NoEffect result) = read_place place in
                   result)
              }
        ; span
        };
      let matches = JsAst.gen_name ~original:None "matches" in
      let_var matches (NoEffect { shape = Bool false; span = None });
      execute
        { shape =
            If
              { condition =
                  { shape =
                      Compare
                        { op = Equal
                        ; lhs =
                            { shape =
                                Field { obj = { shape = Var value; span }; field = "tag" }
                            ; span
                            }
                        ; rhs = { shape = Var (symbol_for label); span }
                        }
                  ; span
                  }
              ; then_case =
                  scope (fun () ->
                    match value_pattern with
                    | None ->
                      assign_var matches (NoEffect { shape = Bool true; span = None })
                    | Some value_pattern ->
                      assign_var
                        matches
                        (does_match
                           value_pattern
                           (place_of
                              (NoEffect
                                 { shape =
                                     Field
                                       { obj = { shape = Var value; span }
                                       ; field = "data"
                                       }
                                 ; span = None
                                 }))))
              ; else_case = None
              }
        ; span
        };
      NoEffect { shape = Var matches; span = None }
    | P_Error -> NoEffect { shape = Bool true; span }

  and pattern_match : pattern -> transpiled_place -> unit =
    fun pattern place ->
    let ctx = Effect.perform GetCtx in
    let span = Some pattern.data.span in
    try
      match pattern.shape with
      | P_Placeholder -> ()
      | P_Ref referenced -> pattern_match referenced (place_from_js (read_place place))
      | P_Unit -> ()
      | P_Binding { bind_mode; binding } ->
        let value =
          match bind_mode with
          | Claim -> claim place
          | ByRef { mut } -> place_to_js place
        in
        (match ctx.mut.module_of_binding |> Id.Map.find_opt binding.id with
         | Some module_var ->
           execute
             { shape =
                 JsAst.Assign
                   { assignee =
                       { shape =
                           Field
                             { obj = { shape = Var module_var; span }
                             ; field = binding.name.name
                             }
                       ; span
                       }
                   ; value = pure value
                   }
             ; span
             }
         | None -> let_var (binding_name ~span:pattern.data.span binding) value)
      | P_Tuple { parts } ->
        let index = ref 0 in
        parts
        |> List.iter (fun (part : pattern tuple_part_of) ->
          match part with
          | Field { label; label_span = _; field = field_pattern } ->
            let member =
              match label with
              | None ->
                let member = Tuple.Member.Index !index in
                index := !index + 1;
                member
              | Some name -> Tuple.Member.Name (Label.get_name name)
            in
            pattern_match field_pattern (field_place place member)
          | Unpack packed -> failwith __LOC__)
      | P_Variant { label = _; label_span = _; value = value_pattern } ->
        (match value_pattern with
         | None -> ()
         | Some value_pattern ->
           pattern_match value_pattern (field_place place (Name "data")))
      | P_Error -> ()
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling pattern matching %a" Span.print pattern.data.span);
      raise e

  and assign : assignee_expr -> transpiled_place -> unit =
    fun assignee place ->
    let span = Some assignee.data.span in
    try
      match assignee.shape with
      | A_Placeholder -> ()
      | A_Unit -> ()
      | A_Tuple { parts } ->
        let index = ref 0 in
        parts
        |> List.iter (fun (part : assignee_expr tuple_part_of) ->
          match part with
          | Field { label; label_span = _; field = field_assignee } ->
            let member =
              match label with
              | None ->
                let member = Tuple.Member.Index !index in
                index := !index + 1;
                member
              | Some name -> Tuple.Member.Name (Label.get_name name)
            in
            assign field_assignee (field_place place member)
          | Unpack packed -> failwith __LOC__)
      | A_Place assignee_place ->
        let assignee_place = transpile_place_expr assignee_place in
        assign_to_place assignee_place (claim place)
      | A_Let pattern -> pattern_match pattern place
      | A_Error -> ()
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling assign %a" Span.print assignee.data.span);
      raise e

  and field_place (place : transpiled_place) (field : Tuple.member) : transpiled_place =
    place_of
      (NoEffect
         { shape =
             JsAst.Field
               { obj =
                   (let (NoEffect obj) = read_place place in
                    obj)
               ; field = tuple_field_name field
               }
         ; span = None
         })

  and transpile_place : place -> transpiled_place =
    fun captured_place ->
    let ctx = Effect.perform GetCtx in
    let value = Kast_interpreter.read_place ~span:ctx.span captured_place in
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked blocked) -> transpile_blocked blocked
    | _ ->
      (match ctx.mut.captured_places |> Id.Map.find_opt captured_place.id with
       | Some place -> place
       | None ->
         let value_name = JsAst.gen_name ~original:None "place_value" in
         let place = place_of_var value_name in
         ctx.mut.captured_places
         <- ctx.mut.captured_places |> Id.Map.add captured_place.id place;
         let value = transpile_value value in
         prepend_late
           [ { shape = Let { var = value_name; value = pure value }; span = None } ];
         place)

  and transpile_place_expr : place_expr -> transpiled_place =
    fun expr ->
    let span = Some expr.data.span in
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | PE_Binding binding ->
        (match ctx.captured |> Kast_interpreter.Scope.find_opt binding.name with
         | Some place -> transpile_place place
         | None -> transpile_binding ~span binding)
      | PE_Field { obj; field; field_span = _ } ->
        let member =
          match field with
          | Index i -> Tuple.Member.Index i
          | Name name -> Tuple.Member.Name (Label.get_name name)
          | Expr _ -> failwith __LOC__
        in
        let obj = transpile_place_expr obj in
        field_place obj member
      | PE_Deref expr ->
        let var = JsAst.gen_name ~original:None "ref" in
        execute { shape = Let { var; value = transpile_expr expr |> pure }; span };
        place_from_js_var var
      | PE_Temp expr ->
        let var = JsAst.gen_name ~original:None "temp" in
        execute { shape = Let { var; value = transpile_expr expr |> pure }; span };
        place_of_var var
      | PE_Error -> failwith __LOC__
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling place expr %a" Span.print expr.data.span);
      raise e

  and prepend (stmts : JsAst.stmt list) : unit =
    let ctx = Effect.perform GetCtx in
    ctx.mut.prepend <- ctx.mut.prepend @ stmts

  and prepend_late (stmts : JsAst.stmt list) : unit =
    let ctx = Effect.perform GetCtx in
    ctx.mut.prepend_late <- ctx.mut.prepend_late @ stmts

  and impl_cast ~value ~target ~impl : unit =
    execute
      { shape =
          Expr
            { shape =
                Call
                  { async = false
                  ; f = { shape = Raw "Kast.casts.add_impl"; span = None }
                  ; args =
                      [ { shape =
                            Obj
                              [ Field { name = "value"; value = pure value }
                              ; Field { name = "target"; value = pure target }
                              ; Field { name = "impl"; value = pure impl }
                              ]
                        ; span = None
                        }
                      ]
                  }
            ; span = None
            }
      ; span = None
      }

  and impl_as_module ~value ~impl ~ty : unit =
    let ty =
      ty
      |> Ty.await_inferred
      |> Ty.Shape.expect_tuple
      |> Option.unwrap_or_else (fun () -> fail "impl as module not tuple??")
    in
    ty.tuple
    |> Tuple.iter (fun member _ ->
      let field = tuple_field_name member in
      execute
        { shape =
            JsAst.Assign
              { assignee = { shape = Field { obj = pure value; field }; span = None }
              ; value = { shape = Field { obj = pure impl; field }; span = None }
              }
        ; span = None
        })

  and symbol_for (label : Label.t) : JsAst.name =
    let id = (Label.get_data label).id in
    let ctx = Effect.perform GetCtx in
    match ctx.mut.symbols |> Id.Map.find_opt id with
    | None ->
      let name = JsAst.gen_name ~original:None "symbol" in
      prepend
        [ { shape =
              Let
                { var = name
                ; value =
                    { shape = Raw (make_string "Symbol(%S)" (Label.get_name label))
                    ; span = None
                    }
                }
          ; span = None
          }
        ];
      ctx.mut.symbols <- ctx.mut.symbols |> Id.Map.add id name;
      name
    | Some name -> name

  and raise_error message =
    execute
      { shape =
          Throw
            { shape =
                Call
                  { async = false
                  ; f = { shape = Raw "Error"; span = None }
                  ; args = [ { shape = String message; span = None } ]
                  }
            ; span = None
            }
      ; span = None
      }

  and pure : no_effect_expr -> JsAst.expr = fun (NoEffect expr) -> expr

  and calculate : JsAst.expr -> no_effect_expr =
    fun value ->
    let var = JsAst.gen_name ~original:None "var" in
    execute { shape = Let { var; value }; span = None };
    NoEffect { shape = Var var; span = None }

  and scope (f : unit -> unit) : JsAst.stmt list =
    let scope : scope = { stmts = [] } in
    (try f () with
     | effect GetScope, k -> Effect.continue k scope);
    scope.stmts

  and assign_var (var : JsAst.name) (value : no_effect_expr) : unit =
    execute
      { shape = Assign { assignee = { shape = Var var; span = None }; value = pure value }
      ; span = None
      }

  and let_var (var : JsAst.name) (value : no_effect_expr) : unit =
    execute { shape = Let { var; value = pure value }; span = None }

  and undefined () = NoEffect { shape = Undefined; span = None }

  and labelled_block label block =
    execute
      { shape = Labelled { label; stmt = { shape = Block (scope block); span = None } }
      ; span = None
      }

  and context_ty_field : value_context_ty -> string =
    fun { id; ty = _ } -> id.value |> Int.to_string

  and create_variant (label : Label.t) (value : no_effect_expr option) : no_effect_expr =
    calculate
      { shape =
          JsAst.Obj
            [ Field
                { name = "tag"
                ; value = { shape = JsAst.Var (symbol_for label); span = None }
                }
            ; Field
                { name = "data"
                ; value =
                    (match value with
                     | Some value -> pure value
                     | None -> { shape = JsAst.Undefined; span = None })
                }
            ]
      ; span = None
      }

  and transpile_expr : expr -> no_effect_expr =
    fun expr ->
    try
      let span = Some expr.data.span in
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | E_Constant { id = _; value } -> transpile_value value
      | E_Ref { mut; place } ->
        let place = transpile_place_expr place in
        place_to_js place
      | E_Claim place ->
        let place = transpile_place_expr place in
        claim place
      | E_Then { list } ->
        let expr = ref None in
        list
        |> List.iter (fun e ->
          (match !expr with
           | None -> ()
           | Some stmt ->
             let (NoEffect _) = transpile_expr stmt in
             ());
          expr := Some e);
        (match !expr with
         | None -> undefined ()
         | Some expr -> transpile_expr expr)
      | E_Stmt { expr } ->
        let (NoEffect _) = transpile_expr expr in
        undefined ()
      | E_Scope { expr } ->
        (* TODO js scope? *)
        transpile_expr expr
      | E_Fn { ty = _; def } -> fn ~captured:None def
      | E_Generic { def; ty = _ } -> fn ~captured:None def
      | E_Tuple { parts } ->
        let idx = ref 0 in
        calculate
          { shape =
              JsAst.Obj
                (parts
                 |> List.map (fun (part : expr tuple_part_of) : JsAst.obj_part ->
                   match part with
                   | Field { label; label_span = _; field = value } ->
                     let member =
                       match label with
                       | None ->
                         let member = Tuple.Member.Index !idx in
                         idx := !idx + 1;
                         member
                       | Some label -> Name (Label.get_name label)
                     in
                     Field
                       { name = tuple_field_name member
                       ; value = pure <| transpile_expr value
                       }
                   | Unpack packed ->
                     (match
                        packed.data.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple
                      with
                      | Some { name = _; tuple } ->
                        if tuple.unnamed |> Array.length <> 0
                        then fail "todo unpack unnamed";
                        Unpack (pure <| transpile_expr packed)
                      | None -> fail "packed ty not a tuple???")))
          ; span
          }
      | E_Variant { label; label_span = _; value } ->
        create_variant label (value |> Option.map transpile_expr)
      | E_Apply { f; arg } ->
        let f = pure <| transpile_expr f in
        let arg = pure <| transpile_expr arg in
        calculate
          { shape =
              JsAst.Call
                { async = async_fns
                ; f
                ; args = [ arg; { shape = Var ctx_var; span = None } ]
                }
          ; span
          }
      | E_InstantiateGeneric { generic; arg } ->
        let generic = pure <| transpile_expr generic in
        let arg = pure <| transpile_expr arg in
        calculate
          { shape =
              JsAst.Call
                { async = async_fns
                ; f = generic
                ; args = [ arg; { shape = Var ctx_var; span = None } ]
                }
          ; span
          }
      | E_Assign { assignee; value } ->
        let place = transpile_place_expr value in
        assign assignee place;
        undefined ()
      | E_Ty _ -> NoEffect { shape = JsAst.Null; span }
      | E_Newtype _ -> NoEffect { shape = JsAst.Null; span }
      | E_Native { id = _; expr } -> calculate { shape = JsAst.Raw expr; span }
      | E_Module { def; bindings } ->
        let module_var = JsAst.gen_name ~original:None "module" in
        bindings
        |> List.iter (fun (binding : binding) ->
          ctx.mut.module_of_binding
          <- ctx.mut.module_of_binding |> Id.Map.add binding.id module_var);
        execute
          { shape = JsAst.Let { var = module_var; value = { shape = Obj []; span } }
          ; span
          };
        let (NoEffect _) = transpile_expr def in
        NoEffect { shape = Var module_var; span }
      | E_UseDotStar { used; bindings } ->
        let used_var = JsAst.gen_name ~original:None "used" in
        let_var used_var (transpile_expr used);
        bindings
        |> List.iter (fun (binding : binding) ->
          let_var
            (binding_name ~span:binding.span binding)
            (NoEffect
               { shape =
                   JsAst.Field
                     { obj = { shape = JsAst.Var used_var; span }
                     ; field = binding.name.name
                     }
               ; span
               }));
        undefined ()
      | E_If { cond; then_case; else_case } ->
        let result = JsAst.gen_name ~original:None "if_result" in
        let condition = transpile_expr cond in
        let_var result (undefined ());
        execute
          { shape =
              If
                { condition = pure condition
                ; then_case =
                    scope (fun () -> assign_var result (transpile_expr then_case))
                ; else_case =
                    Some (scope (fun () -> assign_var result (transpile_expr else_case)))
                }
          ; span
          };
        NoEffect { shape = Var result; span }
      | E_And { lhs; rhs } ->
        let lhs = pure <| transpile_expr lhs in
        let rhs = pure <| transpile_expr rhs in
        calculate { shape = BinOp { op = And; lhs; rhs }; span }
      | E_Or { lhs; rhs } ->
        let lhs = pure <| transpile_expr lhs in
        let rhs = pure <| transpile_expr rhs in
        calculate { shape = BinOp { op = Or; lhs; rhs }; span }
      | E_Match { value; branches } ->
        let value = transpile_place_expr value in
        let result = JsAst.gen_name ~original:None "match_result" in
        let_var result (undefined ());
        let match_label = JsAst.gen_name ~original:None "match" in
        labelled_block match_label (fun () ->
          branches
          |> List.iter (fun { pattern; body } ->
            let condition = does_match pattern value in
            let then_case =
              scope (fun () ->
                pattern_match pattern value;
                assign_var result (transpile_expr body);
                execute { shape = JsAst.LabelledBreak match_label; span = None })
            in
            execute
              { shape =
                  JsAst.If { condition = pure condition; then_case; else_case = None }
              ; span
              });
          let (NoEffect _) =
            calculate
              { shape =
                  Call
                    { async = false
                    ; f = { shape = Raw "console.error"; span = None }
                    ; args =
                        [ { shape = String "unmatched value:"; span = None }
                        ; (let (NoEffect value) = read_place value in
                           value)
                        ]
                    }
              ; span = None
              }
          in
          raise_error
            (make_string "pattern match non exhaustive at %a" Span.print expr.data.span));
        NoEffect { shape = Var result; span = None }
      | E_QuoteAst _ -> failwith __LOC__
      | E_Loop { body } ->
        execute
          { shape =
              For
                { init = None
                ; cond = None
                ; after = None
                ; body =
                    scope (fun () ->
                      let (NoEffect _) = transpile_expr body in
                      ())
                }
          ; span
          };
        undefined ()
      | E_Unwindable { token; body } ->
        let token_var = JsAst.gen_name ~original:None "unwind_token" in
        let catch_var = JsAst.gen_name ~original:None "e" in
        let result = JsAst.gen_name ~original:None "result" in
        let_var token_var (calculate { shape = Raw "Symbol()"; span });
        let_var result (NoEffect { shape = Undefined; span = None });
        execute
          { shape =
              Try
                { body =
                    scope (fun () ->
                      pattern_match token (place_of_var token_var);
                      assign_var result (transpile_expr body))
                ; catch_var
                ; catch_body =
                    scope (fun () ->
                      execute
                        { shape =
                            If
                              { condition =
                                  { shape =
                                      Compare
                                        { op = Equal
                                        ; lhs =
                                            { shape =
                                                Field
                                                  { obj = { shape = Var catch_var; span }
                                                  ; field = "unwind_token"
                                                  }
                                            ; span
                                            }
                                        ; rhs = { shape = Var token_var; span }
                                        }
                                  ; span
                                  }
                              ; then_case =
                                  scope (fun () ->
                                    assign_var
                                      result
                                      (NoEffect
                                         { shape =
                                             Field
                                               { obj = { shape = Var catch_var; span }
                                               ; field = "value"
                                               }
                                         ; span
                                         }))
                              ; else_case =
                                  Some
                                    [ { shape = Throw { shape = Var catch_var; span }
                                      ; span
                                      }
                                    ]
                              }
                        ; span
                        })
                }
          ; span
          };
        NoEffect { shape = Var result; span = None }
      | E_Unwind { token; value } ->
        let token = transpile_expr token in
        let value = transpile_expr value in
        execute
          { shape =
              Throw
                { shape =
                    Obj
                      [ Field { name = "unwind_token"; value = pure token }
                      ; Field { name = "value"; value = pure value }
                      ]
                ; span
                }
          ; span
          };
        undefined ()
      | E_InjectContext { context_ty; value } ->
        let value = transpile_expr value in
        execute
          { shape =
              Assign
                { assignee =
                    { shape =
                        Field
                          { obj = { shape = Var ctx_var; span = None }
                          ; field = context_ty_field context_ty
                          }
                    ; span = None
                    }
                ; value = pure value
                }
          ; span
          };
        undefined ()
      | E_CurrentContext { context_ty } ->
        calculate
          { shape =
              Field
                { obj = { shape = Var ctx_var; span = None }
                ; field = context_ty_field context_ty
                }
          ; span
          }
      | E_ImplCast { value; target; impl } ->
        let value = transpile_expr value in
        let target = transpile_value target in
        let impl = transpile_expr impl in
        impl_cast ~value ~target ~impl;
        undefined ()
      | E_Cast { value; target } ->
        let value = transpile_expr value in
        let target = transpile_value target in
        calculate
          { shape =
              JsAst.Call
                { async = false
                ; f = { shape = JsAst.Raw "Kast.casts.get_impl"; span }
                ; args =
                    [ { shape =
                          Obj
                            [ Field { name = "value"; value = pure value }
                            ; Field { name = "target"; value = pure target }
                            ]
                      ; span
                      }
                    ]
                }
          ; span
          }
      | E_TargetDependent target_dependent ->
        let branch =
          Kast_interpreter.find_target_dependent_branch
            ctx.interpreter
            target_dependent
            ctx.target
        in
        (match branch with
         | Some branch -> transpile_expr branch.body
         | None ->
           todo "no js cfg branch at %a" Span.print expr.data.span;
           undefined ())
      | E_Error -> failwith __LOC__
    with
    | e ->
      Log.error (fun log -> log "While transpiling expr %a" Span.print expr.data.span);
      raise e
  ;;
end

type result = { print : Writer.t -> unit }

let with_ctx ~state ~span f =
  let ctx : ctx =
    { interpreter = state
    ; captured = state.scope
    ; mut =
        { captured_values = ValueMap.empty
        ; captured_places = Id.Map.empty
        ; symbols = Id.Map.empty
        ; prepend = []
        ; prepend_late = []
        ; module_of_binding = Id.Map.empty
        }
    ; span
    ; target = { name = "javascript" }
    }
  in
  let user_code =
    try
      Impl.with_global_scope (fun () ->
        ctx.interpreter.cast_impls.map
        |> ValueMap.iter (fun target impls ->
          let target = Impl.transpile_value target in
          impls
          |> ValueMap.iter (fun value impl ->
            let value = Impl.transpile_value value in
            let impl = Impl.transpile_value impl in
            Impl.impl_cast ~value ~target ~impl));
        ctx.interpreter.cast_impls.as_module
        |> ValueMap.iter (fun value impl ->
          let impl_ty = Value.ty_of impl in
          let value = Impl.transpile_value value in
          let impl = Impl.transpile_value impl in
          Impl.impl_as_module ~value ~impl ~ty:impl_ty));
      Impl.scope f
    with
    | effect GetCtx, k -> Effect.continue k ctx
  in
  let ast : JsAst.stmt list =
    [ ({ shape = Let { var = ctx_var; value = { shape = Obj []; span = None } }
       ; span = None
       }
       : JsAst.stmt)
    ]
    @ ctx.mut.prepend
    @ ctx.mut.prepend_late
    @ user_code
    @ [ { shape = Raw "await Kast.cleanup()"; span = None } ]
  in
  let ast = ast |> Optimizer.optimize_stmts in
  { print =
      (fun writer ->
        writer |> Writer.write_string [%include_file "runtime.js"];
        JsAst.print_toplevel_stmts writer ast)
  }
;;

let transpile_value : state:interpreter_state -> span:span -> value -> result =
  fun ~state ~span value ->
  with_ctx ~state ~span (fun () ->
    let (NoEffect _) = Impl.transpile_value value in
    ())
;;

let transpile_expr : state:interpreter_state -> span:span -> expr -> result =
  fun ~state ~span expr ->
  with_ctx ~state ~span (fun () ->
    let (NoEffect _) = Impl.transpile_expr expr in
    ())
;;
