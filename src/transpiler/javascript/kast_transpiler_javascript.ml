open Std
open Kast_util
open Kast_types
open Types
module JsAst = Javascript_ast
module Base64_vlq = Base64_vlq
module Writer = Writer

type mutable_ctx =
  { mutable captured_values : JsAst.name ValueMap.t
  ; mutable captured_places : JsAst.name Id.Map.t
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

type _ Effect.t += GetCtx : ctx Effect.t

module Impl = struct
  let rec _unused () = ()

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

  and todo_stmt f =
    Format.ksprintf
      (fun s ->
         Log.error (fun log -> log "%s" s);
         raise_error s)
      f

  and todo_expr : 'a. ('a, formatter, unit, JsAst.expr) format4 -> 'a =
    fun f ->
    Format.kdprintf
      (fun p ->
         Log.error (fun log -> log "%t" p);
         scope [ raise_error (make_string "%t" p) ])
      f

  and tuple_field_name : Tuple.member -> string = function
    | Name name -> name
    | Index i -> Int.to_string i

  and fn ~(captured : interpreter_scope option) (def : maybe_compiled_fn) : JsAst.expr =
    let ctx = Effect.perform GetCtx in
    let compiled = def |> Kast_interpreter.await_compiled ~span:ctx.span in
    let { arg; body; evaled_result = _ } =
      compiled |> Option.unwrap_or_else (fun () -> fail "fn not compiled")
    in
    let ctx =
      match captured with
      | None -> ctx
      | Some captured -> { ctx with captured }
    in
    let arg_name = JsAst.gen_name ~original:None "arg" in
    try
      { shape =
          JsAst.Fn
            { async = true
            ; args = [ arg_name ]
            ; body =
                pattern_match arg (place_of { shape = JsAst.Var arg_name; span = None })
                @ [ { shape = JsAst.Return (transpile_expr body); span = None } ]
            }
      ; span = None
      }
    with
    | effect GetCtx, k -> Effect.continue k ctx

  and transpile_ty : ty -> JsAst.expr =
    fun ty ->
    Inference.Var.setup_default_if_needed ty.var;
    match ty.var |> Inference.Var.inferred_opt with
    | None -> not_inferred ty.var
    | Some shape -> shape |> transpile_ty_shape

  and transpile_ty_shape : ty_shape -> JsAst.expr =
    fun shape ->
    let todo_ty s : JsAst.expr =
      { shape = JsAst.Raw (make_string "Kast.types.todo(%S)" s); span = None }
    in
    let primitive s : JsAst.expr =
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
    | T_Variant _ -> todo_ty __LOC__
    | T_Tuple _ -> todo_ty __LOC__
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
    | T_Error -> { shape = JsAst.Null; span = None }

  and transpile_binding : span:span option -> binding -> JsAst.expr =
    fun ~span binding ->
    let ctx = Effect.perform GetCtx in
    match ctx.mut.module_of_binding |> Id.Map.find_opt binding.id with
    | Some module_var ->
      { shape =
          Field { obj = { shape = Var module_var; span }; field = binding.name.name }
      ; span
      }
    | None ->
      let binding_span =
        match span with
        | Some span -> span
        | None -> binding.span
      in
      { shape = Var (binding_name ~span:binding_span binding); span }

  and transpile_blocked : blocked_value -> JsAst.expr =
    fun value ->
    let ctx = Effect.perform GetCtx in
    match value.shape with
    | BV_Binding binding -> transpile_binding ~span:None binding
    | BV_Instantiate _ -> failwith __LOC__
    | BV_ClaimRef _ -> failwith __LOC__
    | BV_FieldRef _ -> failwith __LOC__

  and transpile_value : value -> JsAst.expr =
    fun value ->
    let ctx = Effect.perform GetCtx in
    Inference.Var.setup_default_if_needed value.var;
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked value) -> value |> transpile_blocked
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
        prepend
          [ { shape =
                JsAst.Let
                  { var = value_name
                  ; value =
                      (match value.var |> Inference.Var.inferred_opt with
                       | None -> not_inferred value.var
                       | Some value_shape -> value_shape |> transpile_value_shape)
                  }
            ; span = None
            }
          ]);
      (* TODO copy? *)
      { shape = Var value_name; span = None }

  and transpile_value_shape : value_shape -> JsAst.expr =
    fun shape ->
    try
      let ctx = Effect.perform GetCtx in
      match shape with
      | V_Unit -> { shape = JsAst.Null; span = None }
      | V_Bool x -> { shape = JsAst.Bool x; span = None }
      | V_Int32 x -> { shape = JsAst.Number (Int32.to_float x); span = None }
      | V_Int64 x -> { shape = JsAst.Number (Int64.to_float x); span = None }
      | V_Float64 x -> { shape = JsAst.Number x; span = None }
      | V_Char c -> { shape = JsAst.String (String.make 1 c); span = None }
      | V_String s -> { shape = JsAst.String s; span = None }
      | V_Ref _ -> failwith __LOC__
      | V_Tuple { tuple; ty = _ } ->
        { shape =
            JsAst.Obj
              (tuple
               |> Tuple.to_seq
               |> Seq.map (fun (member, (field : value_tuple_field)) : JsAst.obj_part ->
                 let field_name = tuple_field_name member in
                 let value = field.place |> Kast_interpreter.read_place ~span:ctx.span in
                 Field { name = field_name; value = transpile_value value })
               |> List.of_seq)
        ; span = None
        }
      | V_Variant _ -> failwith __LOC__
      | V_Ty ty -> transpile_ty ty
      | V_Fn { ty = _; fn = { def; captured; _ } } -> fn ~captured:(Some captured) def
      | V_Generic { name = _; fn = { def; captured; _ }; ty = _ } ->
        fn ~captured:(Some captured) def
      | V_NativeFn _ -> failwith __LOC__
      | V_Ast _ -> failwith __LOC__
      | V_UnwindToken _ -> failwith __LOC__
      | V_Target _ -> failwith __LOC__
      | V_ContextTy _ -> { shape = JsAst.Null; span = None }
      | V_CompilerScope _ -> { shape = JsAst.Undefined; span = None }
      | V_Opaque _ -> failwith __LOC__
      | V_Blocked _ -> failwith __LOC__
      | V_Error -> { shape = JsAst.Undefined; span = None }
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

  and claim : JsAst.expr -> JsAst.expr =
    fun place ->
    { shape =
        JsAst.Call
          { async = false
          ; f = { shape = JsAst.Field { obj = place; field = "get" }; span = None }
          ; args = []
          }
    ; span = None
    }

  and read_place : JsAst.expr -> JsAst.expr =
    fun place ->
    { shape =
        JsAst.Call
          { async = false
          ; f = { shape = JsAst.Field { obj = place; field = "get" }; span = None }
          ; args = []
          }
    ; span = None
    }

  and does_match (pattern : pattern) (place : JsAst.expr) : JsAst.expr =
    let span = Some pattern.data.span in
    match pattern.shape with
    | P_Placeholder -> { shape = Bool true; span }
    | P_Ref referenced_pattern -> does_match referenced_pattern (read_place place)
    | P_Unit -> { shape = Bool true; span }
    | P_Binding _ -> { shape = Bool true; span }
    | P_Tuple { parts } ->
      scope
        (let index = ref 0 in
         (parts
          |> List.map (fun (part : pattern tuple_part_of) : JsAst.stmt ->
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
              { shape =
                  JsAst.If
                    { condition =
                        { shape =
                            Not (does_match field_pattern (field_place place member))
                        ; span
                        }
                    ; then_case =
                        [ { shape = Return { shape = Bool false; span }; span } ]
                    ; else_case = None
                    }
              ; span
              }
            | Unpack packed -> failwith __LOC__))
         @ [ { shape = Return { shape = Bool true; span }; span } ])
    | P_Variant { label; label_span = _; value = value_pattern } ->
      let var = JsAst.gen_name ~original:None "variant" in
      scope
        [ { shape = Let { var; value = read_place place }; span }
        ; { shape =
              If
                { condition =
                    { shape =
                        Compare
                          { op = Equal
                          ; lhs =
                              { shape =
                                  Field { obj = { shape = Var var; span }; field = "tag" }
                              ; span
                              }
                          ; rhs = { shape = Var (symbol_for label); span }
                          }
                    ; span
                    }
                ; then_case =
                    (match value_pattern with
                     | None -> [ { shape = Return { shape = Bool true; span }; span } ]
                     | Some value_pattern ->
                       [ { shape =
                             Return
                               (does_match
                                  value_pattern
                                  (place_of
                                     { shape =
                                         Field
                                           { obj = { shape = Var var; span }
                                           ; field = "data"
                                           }
                                     ; span = None
                                     }))
                         ; span
                         }
                       ])
                ; else_case =
                    Some [ { shape = Return { shape = Bool false; span }; span } ]
                }
          ; span
          }
        ]
    | P_Error -> { shape = Bool true; span }

  and pattern_match : pattern -> JsAst.expr -> JsAst.stmt list =
    fun pattern place ->
    let ctx = Effect.perform GetCtx in
    let span = Some pattern.data.span in
    try
      match pattern.shape with
      | P_Placeholder -> []
      | P_Ref referenced -> pattern_match referenced (read_place place)
      | P_Unit -> []
      | P_Binding { bind_mode; binding } ->
        [ { shape =
              (let value =
                 match bind_mode with
                 | Claim -> claim place
                 | ByRef { mut } -> place
               in
               match ctx.mut.module_of_binding |> Id.Map.find_opt binding.id with
               | Some module_var ->
                 JsAst.Assign
                   { assignee =
                       { shape =
                           Field
                             { obj = { shape = Var module_var; span }
                             ; field = binding.name.name
                             }
                       ; span
                       }
                   ; value
                   }
               | None ->
                 JsAst.Let { var = binding_name ~span:pattern.data.span binding; value })
          ; span
          }
        ]
      | P_Tuple { parts } ->
        let index = ref 0 in
        parts
        |> List.map (fun (part : pattern tuple_part_of) ->
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
        |> List.flatten
      | P_Variant { label = _; label_span = _; value = value_pattern } ->
        (match value_pattern with
         | None -> []
         | Some value_pattern ->
           pattern_match
             value_pattern
             (place_of
                { shape = Field { obj = read_place place; field = "data" }; span = None }))
      | P_Error -> []
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling pattern matching %a" Span.print pattern.data.span);
      raise e

  and assign : assignee_expr -> JsAst.expr -> JsAst.stmt list =
    fun assignee place ->
    let span = Some assignee.data.span in
    try
      match assignee.shape with
      | A_Placeholder -> []
      | A_Unit -> []
      | A_Tuple { parts } ->
        let index = ref 0 in
        parts
        |> List.map (fun (part : assignee_expr tuple_part_of) ->
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
        |> List.flatten
      | A_Place assignee_place ->
        let assignee_place = transpile_place_expr assignee_place in
        [ { shape =
              Expr
                { shape =
                    Call
                      { async = false
                      ; f =
                          { shape = Field { obj = assignee_place; field = "set" }; span }
                      ; args = [ claim place ]
                      }
                ; span
                }
          ; span
          }
        ]
      | A_Let pattern -> pattern_match pattern place
      | A_Error -> []
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling assign %a" Span.print assignee.data.span);
      raise e

  and stmts_expr : JsAst.stmt list -> JsAst.expr =
    fun stmts ->
    { shape =
        JsAst.Call
          { async = true
          ; f =
              { shape = JsAst.Fn { async = true; args = []; body = stmts }; span = None }
          ; args = []
          }
    ; span = None
    }

  and place ~get ~set : JsAst.expr =
    { shape =
        JsAst.Obj
          [ Field { name = "get"; value = get }; Field { name = "set"; value = set } ]
    ; span = None
    }

  and place_of (expr : JsAst.expr) : JsAst.expr =
    place
      ~get:
        { shape =
            JsAst.Fn
              { async = false
              ; args = []
              ; body = [ { shape = JsAst.Return expr; span = None } ]
              }
        ; span = None
        }
      ~set:
        (let var = JsAst.gen_name ~original:None "value" in
         { shape =
             JsAst.Fn
               { async = false
               ; args = [ var ]
               ; body =
                   [ { shape =
                         JsAst.Assign
                           { assignee = expr
                           ; value = { shape = JsAst.Var var; span = None }
                           }
                     ; span = None
                     }
                   ]
               }
         ; span = None
         })

  and scope (body : JsAst.stmt list) : JsAst.expr =
    { shape =
        JsAst.Call
          { async = true
          ; f = { shape = JsAst.Fn { async = true; args = []; body }; span = None }
          ; args = []
          }
    ; span = None
    }

  and field_place (place : JsAst.expr) (field : Tuple.member) : JsAst.expr =
    place_of
      { shape = JsAst.Field { obj = read_place place; field = tuple_field_name field }
      ; span = None
      }

  and transpile_place : place -> JsAst.expr =
    fun captured_place ->
    let ctx = Effect.perform GetCtx in
    let value = Kast_interpreter.read_place ~span:ctx.span captured_place in
    match value.var |> Inference.Var.inferred_opt with
    | Some (V_Blocked blocked) -> place_of (transpile_blocked blocked)
    | _ ->
      let var =
        match ctx.mut.captured_places |> Id.Map.find_opt captured_place.id with
        | Some name -> name
        | None ->
          let value_name = JsAst.gen_name ~original:None "place_value" in
          let place_name = JsAst.gen_name ~original:None "place" in
          ctx.mut.captured_places
          <- ctx.mut.captured_places |> Id.Map.add captured_place.id place_name;
          let place = place_of { shape = Var value_name; span = None } in
          prepend [ { shape = Let { var = place_name; value = place }; span = None } ];
          let value = transpile_value value in
          prepend_late [ { shape = Let { var = value_name; value }; span = None } ];
          place_name
      in
      { shape = Var var; span = None }

  and transpile_place_expr : place_expr -> JsAst.expr =
    fun expr ->
    let span = Some expr.data.span in
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | PE_Binding binding ->
        (match ctx.captured |> Kast_interpreter.Scope.find_opt binding.name with
         | Some place -> transpile_place place
         | None -> place_of (transpile_binding ~span binding))
      | PE_Field { obj; field; field_span = _ } ->
        let var = JsAst.gen_name ~original:None "obj" in
        let member =
          match field with
          | Index i -> Tuple.Member.Index i
          | Name name -> Tuple.Member.Name (Label.get_name name)
          | Expr _ -> failwith __LOC__
        in
        scope
          [ { shape = JsAst.Let { var; value = transpile_place_expr obj }; span }
          ; { shape = JsAst.Return (field_place { shape = JsAst.Var var; span } member)
            ; span
            }
          ]
      | PE_Deref expr -> transpile_expr expr
      | PE_Temp expr ->
        let var = JsAst.gen_name ~original:None "temp" in
        scope
          [ { shape = JsAst.Let { var; value = transpile_expr expr }; span }
          ; { shape = JsAst.Return (place_of { shape = JsAst.Var var; span }); span }
          ]
      | PE_Error -> failwith __LOC__
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling place expr %a" Span.print expr.data.span);
      raise e

  and transpile_expr_as_stmts : expr -> JsAst.stmt list =
    fun expr ->
    let span = Some expr.data.span in
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | E_Constant _ -> []
      | E_Ref _ -> failwith __LOC__
      | E_Claim _ -> failwith __LOC__
      | E_Then { list } -> list |> List.map transpile_expr_as_stmts |> List.flatten
      | E_Stmt { expr } -> transpile_expr_as_stmts expr
      | E_Scope _ -> [ { shape = Expr (transpile_expr expr); span } ]
      | E_Fn _ -> failwith __LOC__
      | E_Generic _ -> failwith __LOC__
      | E_Tuple _ -> failwith __LOC__
      | E_Variant _ -> failwith __LOC__
      | E_Apply _ -> [ { shape = Expr (transpile_expr expr); span } ]
      | E_InstantiateGeneric _ -> failwith __LOC__
      | E_Assign { assignee; value } ->
        let var = JsAst.gen_name ~original:None "var" in
        [ ({ shape = JsAst.Let { var; value = transpile_place_expr value }; span }
           : JsAst.stmt)
        ]
        @ assign assignee { shape = JsAst.Var var; span }
      | E_Ty _ -> failwith __LOC__
      | E_Newtype _ -> failwith __LOC__
      | E_Native _ -> failwith __LOC__
      | E_Module _ -> failwith __LOC__
      | E_UseDotStar { used; bindings } ->
        let used_var = JsAst.gen_name ~original:None "used" in
        [ ({ shape = JsAst.Let { var = used_var; value = transpile_expr used }; span }
           : JsAst.stmt)
        ]
        @ (bindings
           |> List.map (fun (binding : binding) : JsAst.stmt ->
             { shape =
                 JsAst.Let
                   { var = binding_name ~span:binding.span binding
                   ; value =
                       { shape =
                           JsAst.Field
                             { obj = { shape = JsAst.Var used_var; span }
                             ; field = binding.name.name
                             }
                       ; span
                       }
                   }
             ; span
             }))
      | E_If _ -> [ { shape = Expr (transpile_expr expr); span } ]
      | E_And _ -> failwith __LOC__
      | E_Or _ -> failwith __LOC__
      | E_Match _ -> [ { shape = Expr (transpile_expr expr); span } ]
      | E_QuoteAst _ -> failwith __LOC__
      | E_Loop { body } ->
        [ { shape =
              For
                { init = None
                ; cond = None
                ; after = None
                ; body = transpile_expr_as_stmts body
                }
          ; span
          }
        ]
      | E_Unwindable _ -> [ { shape = Expr (transpile_expr expr); span } ]
      | E_Unwind { token; value } ->
        [ { shape =
              Throw
                { shape =
                    Obj
                      [ Field { name = "unwind_token"; value = transpile_expr token }
                      ; Field { name = "value"; value = transpile_expr value }
                      ]
                ; span
                }
          ; span
          }
        ]
      | E_InjectContext _ -> failwith __LOC__
      | E_CurrentContext _ -> failwith __LOC__
      | E_ImplCast { value; target; impl } ->
        impl_cast
          ~value:(transpile_expr value)
          ~target:(transpile_value target)
          ~impl:(transpile_expr impl)
      | E_Cast _ -> failwith __LOC__
      | E_TargetDependent _ -> failwith __LOC__
      | E_Error -> failwith __LOC__
    with
    | e ->
      Log.error (fun log ->
        log "While transpiling expr as stmts %a" Span.print expr.data.span);
      raise e

  and prepend (stmts : JsAst.stmt list) : unit =
    let ctx = Effect.perform GetCtx in
    ctx.mut.prepend <- ctx.mut.prepend @ stmts

  and prepend_late (stmts : JsAst.stmt list) : unit =
    let ctx = Effect.perform GetCtx in
    ctx.mut.prepend_late <- ctx.mut.prepend_late @ stmts

  and impl_cast ~value ~target ~impl : JsAst.stmt list =
    [ { shape =
          Expr
            { shape =
                Call
                  { async = false
                  ; f = { shape = Raw "Kast.casts.add_impl"; span = None }
                  ; args =
                      [ { shape =
                            Obj
                              [ Field { name = "value"; value }
                              ; Field { name = "target"; value = target }
                              ; Field { name = "impl"; value = impl }
                              ]
                        ; span = None
                        }
                      ]
                  }
            ; span = None
            }
      ; span = None
      }
    ]

  and impl_as_module ~value ~impl ~ty : JsAst.stmt list =
    let ty =
      ty
      |> Ty.await_inferred
      |> Ty.Shape.expect_tuple
      |> Option.unwrap_or_else (fun () -> fail "impl as module not tuple??")
    in
    let value_var = JsAst.gen_name ~original:None "value" in
    let impl_var = JsAst.gen_name ~original:None "impl" in
    [ ({ shape = JsAst.Let { var = value_var; value }; span = None } : JsAst.stmt)
    ; { shape = JsAst.Let { var = impl_var; value = impl }; span = None }
    ]
    @ (ty.tuple
       |> Tuple.to_seq
       |> Seq.map (fun (member, _) : JsAst.stmt ->
         let field = tuple_field_name member in
         { shape =
             JsAst.Assign
               { assignee =
                   { shape = Field { obj = { shape = Var value_var; span = None }; field }
                   ; span = None
                   }
               ; value =
                   { shape = Field { obj = { shape = Var impl_var; span = None }; field }
                   ; span = None
                   }
               }
         ; span = None
         })
       |> List.of_seq)

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

  and raise_error message : JsAst.stmt =
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

  and transpile_expr : expr -> JsAst.expr =
    fun expr ->
    try
      let span = Some expr.data.span in
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | E_Constant value -> transpile_value value
      | E_Ref { mut; place } -> transpile_place_expr place
      | E_Claim expr -> transpile_place_expr expr |> claim
      | E_Then { list } ->
        let body =
          list
          |> List.mapi (fun i e ->
            if i + 1 = List.length list
            then [ ({ shape = JsAst.Return (transpile_expr e); span } : JsAst.stmt) ]
            else transpile_expr_as_stmts e)
          |> List.flatten
        in
        { shape =
            JsAst.Call
              { async = true
              ; f = { shape = JsAst.Fn { async = true; args = []; body }; span }
              ; args = []
              }
        ; span
        }
      | E_Stmt { expr } -> transpile_expr expr
      | E_Scope { expr } ->
        (* TODO should I actually create js scope? *)
        transpile_expr expr
      | E_Fn { ty = _; def } -> fn ~captured:None def
      | E_Generic { def; ty = _ } -> fn ~captured:None def
      | E_Tuple { parts } ->
        let idx = ref 0 in
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
                   Field { name = tuple_field_name member; value = transpile_expr value }
                 | Unpack packed -> failwith __LOC__))
        ; span
        }
      | E_Variant { label; label_span = _; value } ->
        { shape =
            JsAst.Obj
              [ Field
                  { name = "tag"; value = { shape = JsAst.Var (symbol_for label); span } }
              ; Field
                  { name = "data"
                  ; value =
                      (match value with
                       | Some value -> transpile_expr value
                       | None -> { shape = JsAst.Undefined; span })
                  }
              ]
        ; span
        }
      | E_Apply { f; arg } ->
        { shape =
            JsAst.Call
              { async = true; f = transpile_expr f; args = [ transpile_expr arg ] }
        ; span
        }
      | E_InstantiateGeneric { generic; arg } ->
        { shape =
            JsAst.Call
              { async = true; f = transpile_expr generic; args = [ transpile_expr arg ] }
        ; span
        }
      | E_Assign _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_Ty _ -> { shape = JsAst.Null; span }
      | E_Newtype _ -> { shape = JsAst.Null; span }
      | E_Native { id = _; expr } -> { shape = JsAst.Raw expr; span }
      | E_Module { def; bindings } ->
        let module_var = JsAst.gen_name ~original:None "module" in
        bindings
        |> List.iter (fun binding ->
          ctx.mut.module_of_binding
          <- ctx.mut.module_of_binding |> Id.Map.add binding.id module_var);
        let introduce_module : JsAst.stmt =
          { shape = JsAst.Let { var = module_var; value = { shape = Obj []; span } }
          ; span
          }
        in
        scope
          ([ introduce_module ]
           @ transpile_expr_as_stmts def
           @ [ { shape = Return { shape = Var module_var; span }; span } ])
      | E_UseDotStar _ -> failwith __LOC__
      | E_If { cond; then_case; else_case } ->
        scope
          [ { shape =
                If
                  { condition = transpile_expr cond
                  ; then_case = [ { shape = Return (transpile_expr then_case); span } ]
                  ; else_case =
                      Some [ { shape = Return (transpile_expr else_case); span } ]
                  }
            ; span
            }
          ]
      | E_And { lhs; rhs } ->
        { shape = BinOp { op = And; lhs = transpile_expr lhs; rhs = transpile_expr rhs }
        ; span
        }
      | E_Or { lhs; rhs } ->
        { shape = BinOp { op = Or; lhs = transpile_expr lhs; rhs = transpile_expr rhs }
        ; span
        }
      | E_Match { value; branches } ->
        scope
          (let var = JsAst.gen_name ~original:None "matched" in
           let stmts : JsAst.stmt list ref =
             ref
               [ ({ shape = JsAst.Let { var; value = transpile_place_expr value }; span }
                  : JsAst.stmt)
               ]
           in
           stmts
           := !stmts
              @ (branches
                 |> List.map (fun { pattern; body } : JsAst.stmt ->
                   { shape =
                       JsAst.If
                         { condition = does_match pattern { shape = Var var; span }
                         ; then_case =
                             pattern_match pattern { shape = Var var; span }
                             @ [ { shape = JsAst.Return (transpile_expr body); span } ]
                         ; else_case = None
                         }
                   ; span
                   }));
           stmts := !stmts @ [ raise_error "pattern match non exhaustive" ];
           !stmts)
      | E_QuoteAst _ -> failwith __LOC__
      | E_Loop _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_Unwindable { token; body } ->
        let token_var = JsAst.gen_name ~original:None "unwind_token" in
        let catch_var = JsAst.gen_name ~original:None "e" in
        scope
          [ { shape = Let { var = token_var; value = { shape = Raw "Symbol()"; span } }
            ; span
            }
          ; { shape =
                Try
                  { body =
                      pattern_match token (place_of { shape = Var token_var; span })
                      @ [ { shape = Return (transpile_expr body); span } ]
                  ; catch_var
                  ; catch_body =
                      [ { shape =
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
                                  [ { shape =
                                        Return
                                          { shape =
                                              Field
                                                { obj = { shape = Var catch_var; span }
                                                ; field = "value"
                                                }
                                          ; span
                                          }
                                    ; span
                                    }
                                  ]
                              ; else_case = None
                              }
                        ; span
                        }
                      ; { shape = Throw { shape = Var catch_var; span }; span }
                      ]
                  }
            ; span
            }
          ]
      | E_Unwind _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_InjectContext _ -> failwith __LOC__
      | E_CurrentContext _ -> failwith __LOC__
      | E_ImplCast _ -> failwith __LOC__
      | E_Cast { value; target } ->
        let value = transpile_expr value in
        let target = transpile_value target in
        { shape =
            JsAst.Call
              { async = false
              ; f = { shape = JsAst.Raw "Kast.casts.get_impl"; span }
              ; args =
                  [ { shape =
                        Obj
                          [ Field { name = "value"; value }
                          ; Field { name = "target"; value = target }
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
         | None -> todo_expr "no js cfg branch at %a" Span.print expr.data.span)
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
  let result =
    try
      ctx.interpreter.cast_impls.map
      |> ValueMap.iter (fun target impls ->
        impls
        |> ValueMap.iter (fun value impl ->
          Impl.prepend
          <| Impl.impl_cast
               ~value:(Impl.transpile_value value)
               ~target:(Impl.transpile_value target)
               ~impl:(Impl.transpile_value impl)));
      ctx.interpreter.cast_impls.as_module
      |> ValueMap.iter (fun value impl ->
        Impl.prepend
        <| Impl.impl_as_module
             ~value:(Impl.transpile_value value)
             ~impl:(Impl.transpile_value impl)
             ~ty:impl.ty);
      f ()
    with
    | effect GetCtx, k -> Effect.continue k ctx
  in
  let result_var = JsAst.gen_name ~original:None "result" in
  let ast : JsAst.expr =
    Impl.scope
      (ctx.mut.prepend
       @ ctx.mut.prepend_late
       @ [ { shape = Let { var = result_var; value = result }; span = None }
         ; { shape = Raw "await Kast.cleanup()"; span = None }
         ; { shape = Return { shape = Var result_var; span = None }; span = None }
         ])
  in
  let ast : JsAst.expr =
    match ast.shape with
    | Call { async = true; f; args } ->
      { shape = Call { async = false; f; args }; span = ast.span }
    | _ -> failwith __LOC__
  in
  { print =
      (fun writer ->
        writer |> Writer.write_string [%blob "./runtime.js"];
        JsAst.print_expr ~precedence:None writer ast)
  }
;;

let transpile_value : state:interpreter_state -> span:span -> value -> result =
  fun ~state ~span value -> with_ctx ~state ~span (fun () -> Impl.transpile_value value)
;;

let transpile_expr : state:interpreter_state -> span:span -> expr -> result =
  fun ~state ~span expr -> with_ctx ~state ~span (fun () -> Impl.transpile_expr expr)
;;
