open Std
open Kast_util
open Kast_types
open Types
module JsAst = Javascript_ast

type mutable_ctx = {
  mutable values : JsAst.name ValueMap.t;
  mutable symbols : JsAst.name Id.Map.t;
  mutable prepend : JsAst.stmt list;
}

type ctx = {
  interpreter : interpreter_state;
  mut : mutable_ctx;
  captured : interpreter_scope;
  span : span;
  target : value_target;
}

type _ Effect.t += GetCtx : ctx Effect.t

module Impl = struct
  let rec _unused () = ()

  and not_inferred : 'a 'scope. ('a, 'scope) Inference.Var.t -> JsAst.expr =
   fun var ->
    Log.error (fun log -> log "transpiling not inferred var");
    JsAst.Obj
      [
        Field { name = "type"; value = String "not inferred" };
        Field
          {
            name = "spans";
            value =
              List
                (Inference.Var.spans var |> SpanSet.to_list
                |> List.map (fun span ->
                    JsAst.String (make_string "%a" Span.print span)));
          };
      ]

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

  and fn ~(captured : interpreter_scope option) (def : maybe_compiled_fn) :
      JsAst.expr =
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
    let arg_name = JsAst.gen_name "arg" in
    try
      JsAst.Fn
        {
          async = true;
          args = [ arg_name ];
          body =
            pattern_match arg (place_of (JsAst.Var arg_name))
            @ [ JsAst.Return (transpile_expr body) ];
        }
    with effect GetCtx, k -> Effect.continue k ctx

  and transpile_ty : ty -> JsAst.expr =
   fun ty ->
    Inference.Var.setup_default_if_needed ty.var;
    match ty.var |> Inference.Var.inferred_opt with
    | None -> not_inferred ty.var
    | Some shape -> shape |> transpile_ty_shape

  and transpile_ty_shape : ty_shape -> JsAst.expr =
   fun shape ->
    let todo_ty s : JsAst.expr =
      JsAst.Raw (make_string "Kast.types.todo(%S)" s)
    in
    let primitive s : JsAst.expr =
      JsAst.Raw (make_string "Kast.types.primitive[%S]" s)
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
    | T_Error -> JsAst.Null

  and transpile_blocked : blocked_value -> JsAst.expr =
   fun value ->
    match value.shape with
    | BV_Binding binding -> Var (binding_name binding)
    | BV_Instantiate _ -> failwith __LOC__
    | BV_ClaimRef _ -> failwith __LOC__
    | BV_FieldRef _ -> failwith __LOC__

  and transpile_value : value -> JsAst.expr =
   fun value ->
    let ctx = Effect.perform GetCtx in
    Inference.Var.setup_default_if_needed value.var;
    let value_shape = value.var |> Inference.Var.inferred_opt in
    match value_shape with
    | None -> not_inferred value.var
    | Some (V_Blocked value) -> value |> transpile_blocked
    | Some value_shape ->
        let value_name = ref None in
        let do_prepend = ref false in
        ctx.mut.values <-
          ctx.mut.values
          |> ValueMap.update value (fun name ->
              let name =
                match name with
                | Some name -> name
                | None ->
                    let name = JsAst.gen_name "value" in
                    do_prepend := true;
                    name
              in
              value_name := Some name;
              Some name);
        let value_name = !value_name |> Option.get in
        if !do_prepend then (
          Log.trace (fun log -> log "prepend %a" Value.print value);
          prepend
            [
              JsAst.Let
                {
                  var = value_name;
                  value = value_shape |> transpile_value_shape;
                };
            ]);
        (* TODO copy? *)
        Var value_name

  and transpile_value_shape : value_shape -> JsAst.expr =
   fun shape ->
    try
      let ctx = Effect.perform GetCtx in
      match shape with
      | V_Unit -> JsAst.Null
      | V_Bool x -> JsAst.Bool x
      | V_Int32 x -> JsAst.Number (Int32.to_float x)
      | V_Int64 x -> JsAst.Number (Int64.to_float x)
      | V_Float64 x -> JsAst.Number x
      | V_Char c -> JsAst.String (String.make 1 c)
      | V_String s -> JsAst.String s
      | V_Ref _ -> failwith __LOC__
      | V_Tuple { tuple; ty = _ } ->
          JsAst.Obj
            (tuple |> Tuple.to_seq
            |> Seq.map
                 (fun (member, (field : value_tuple_field)) : JsAst.obj_part ->
                   let field_name = tuple_field_name member in
                   let value =
                     field.place |> Kast_interpreter.read_place ~span:ctx.span
                   in
                   Field { name = field_name; value = transpile_value value })
            |> List.of_seq)
      | V_Variant _ -> failwith __LOC__
      | V_Ty ty -> transpile_ty ty
      | V_Fn { ty = _; fn = { def; captured; _ } } ->
          fn ~captured:(Some captured) def
      | V_Generic { name = _; fn = { def; captured; _ }; ty = _ } ->
          fn ~captured:(Some captured) def
      | V_NativeFn _ -> failwith __LOC__
      | V_Ast _ -> failwith __LOC__
      | V_UnwindToken _ -> failwith __LOC__
      | V_Target _ -> failwith __LOC__
      | V_ContextTy _ -> JsAst.Null
      | V_CompilerScope _ -> JsAst.Undefined
      | V_Opaque _ -> failwith __LOC__
      | V_Blocked _ -> failwith __LOC__
      | V_Error -> JsAst.Undefined
    with e ->
      Log.error (fun log ->
          log "While transpiling value shape %a" Value.Shape.print shape);
      raise e

  and binding_name : binding -> JsAst.name =
   fun binding ->
    { raw = make_string "%s_%d" binding.name.name binding.id.value }

  and claim : JsAst.expr -> JsAst.expr =
   fun place ->
    JsAst.Call
      {
        async = false;
        f = JsAst.Field { obj = place; field = "get" };
        args = [];
      }

  and read_place : JsAst.expr -> JsAst.expr =
   fun place ->
    JsAst.Call
      {
        async = false;
        f = JsAst.Field { obj = place; field = "get" };
        args = [];
      }

  and does_match (pattern : pattern) (place : JsAst.expr) : JsAst.expr =
    match pattern.shape with
    | P_Placeholder -> Bool true
    | P_Ref referenced_pattern ->
        does_match referenced_pattern (read_place place)
    | P_Unit -> Bool true
    | P_Binding _ -> Bool true
    | P_Tuple { parts } ->
        scope
          (let index = ref 0 in
           (parts
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
                   JsAst.If
                     {
                       condition =
                         Not
                           (does_match field_pattern (field_place place member));
                       then_case = [ Return (Bool false) ];
                       else_case = None;
                     }
               | Unpack packed -> failwith __LOC__))
           @ [ Return (Bool true) ])
    | P_Variant { label; label_span = _; value = value_pattern } ->
        let var = JsAst.gen_name "variant" in
        scope
          [
            Let { var; value = read_place place };
            If
              {
                condition =
                  Compare
                    {
                      op = Equal;
                      lhs = Field { obj = Var var; field = "tag" };
                      rhs = Var (symbol_for label);
                    };
                then_case =
                  (match value_pattern with
                  | None -> [ Return (Bool true) ]
                  | Some value_pattern ->
                      [
                        Return
                          (does_match value_pattern
                             (place_of
                                (Field { obj = Var var; field = "data" })));
                      ]);
                else_case = Some [ Return (Bool false) ];
              };
          ]
    | P_Error -> Bool true

  and pattern_match : pattern -> JsAst.expr -> JsAst.stmt list =
   fun pattern place ->
    try
      match pattern.shape with
      | P_Placeholder -> []
      | P_Ref referenced -> pattern_match referenced (read_place place)
      | P_Unit -> []
      | P_Binding { bind_mode; binding } ->
          [
            JsAst.Let
              {
                var = binding_name binding;
                value =
                  (match bind_mode with
                  | Claim -> claim place
                  | ByRef { mut } -> place);
              };
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
      | P_Variant { label = _; label_span = _; value = value_pattern } -> (
          match value_pattern with
          | None -> []
          | Some value_pattern ->
              pattern_match value_pattern
                (place_of (Field { obj = read_place place; field = "data" })))
      | P_Error -> []
    with e ->
      Log.error (fun log ->
          log "While transpiling pattern matching %a" Span.print
            pattern.data.span);
      raise e

  and assign : assignee_expr -> JsAst.expr -> JsAst.stmt list =
   fun assignee place ->
    try
      match assignee.shape with
      | A_Placeholder -> []
      | A_Unit -> []
      | A_Tuple _ -> failwith __LOC__
      | A_Place assignee_place ->
          let assignee_place = transpile_place assignee_place in
          [
            Expr
              (Call
                 {
                   async = false;
                   f = Field { obj = assignee_place; field = "set" };
                   args = [ claim place ];
                 });
          ]
      | A_Let pattern -> pattern_match pattern place
      | A_Error -> []
    with e ->
      Log.error (fun log ->
          log "While transpiling assign %a" Span.print assignee.data.span);
      raise e

  and stmts_expr : JsAst.stmt list -> JsAst.expr =
   fun stmts ->
    JsAst.Call
      {
        async = true;
        f = JsAst.Fn { async = true; args = []; body = stmts };
        args = [];
      }

  and place ~get ~set =
    JsAst.Obj
      [
        Field { name = "get"; value = get }; Field { name = "set"; value = set };
      ]

  and place_of expr =
    place
      ~get:(JsAst.Fn { async = false; args = []; body = [ JsAst.Return expr ] })
      ~set:
        (let var = JsAst.gen_name "value" in
         JsAst.Fn
           {
             async = false;
             args = [ var ];
             body = [ JsAst.Assign { assignee = expr; value = JsAst.Var var } ];
           })

  and scope (body : JsAst.stmt list) : JsAst.expr =
    JsAst.Call
      {
        async = true;
        f = JsAst.Fn { async = true; args = []; body };
        args = [];
      }

  and field_place (place : JsAst.expr) (field : Tuple.member) : JsAst.expr =
    place_of
      (JsAst.Field { obj = read_place place; field = tuple_field_name field })

  and transpile_place : place_expr -> JsAst.expr =
   fun expr ->
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | PE_Binding binding -> (
          match
            ctx.captured |> Kast_interpreter.Scope.find_opt binding.name
          with
          | Some place ->
              place_of
                (transpile_value
                   (Kast_interpreter.read_place ~span:ctx.span place))
          | None -> place_of (JsAst.Var (binding_name binding)))
      | PE_Field { obj; field; field_span = _ } ->
          let var = JsAst.gen_name "obj" in
          let member =
            match field with
            | Index i -> Tuple.Member.Index i
            | Name name -> Tuple.Member.Name (Label.get_name name)
            | Expr _ -> failwith __LOC__
          in
          scope
            [
              JsAst.Let { var; value = transpile_place obj };
              JsAst.Return (field_place (JsAst.Var var) member);
            ]
      | PE_Deref expr -> transpile_expr expr
      | PE_Temp expr ->
          let var = JsAst.gen_name "temp" in
          scope
            [
              JsAst.Let { var; value = transpile_expr expr };
              JsAst.Return (place_of (JsAst.Var var));
            ]
      | PE_Error -> failwith __LOC__
    with e ->
      Log.error (fun log ->
          log "While transpiling place expr %a" Span.print expr.data.span);
      raise e

  and transpile_expr_as_stmts : expr -> JsAst.stmt list =
   fun expr ->
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | E_Constant _ -> []
      | E_Ref _ -> failwith __LOC__
      | E_Claim _ -> failwith __LOC__
      | E_Then { list } ->
          list |> List.map transpile_expr_as_stmts |> List.flatten
      | E_Stmt { expr } -> transpile_expr_as_stmts expr
      | E_Scope _ -> [ Expr (transpile_expr expr) ]
      | E_Fn _ -> failwith __LOC__
      | E_Generic _ -> failwith __LOC__
      | E_Tuple _ -> failwith __LOC__
      | E_Variant _ -> failwith __LOC__
      | E_Apply _ -> [ Expr (transpile_expr expr) ]
      | E_InstantiateGeneric _ -> failwith __LOC__
      | E_Assign { assignee; value } ->
          let var = JsAst.gen_name "var" in
          [ JsAst.Let { var; value = transpile_place value } ]
          @ assign assignee (JsAst.Var var)
      | E_Ty _ -> failwith __LOC__
      | E_Newtype _ -> failwith __LOC__
      | E_Native _ -> failwith __LOC__
      | E_Module _ -> failwith __LOC__
      | E_UseDotStar { used; bindings } ->
          let used_var = JsAst.gen_name "used" in
          [ JsAst.Let { var = used_var; value = transpile_expr used } ]
          @ (bindings
            |> List.map (fun binding ->
                JsAst.Let
                  {
                    var = binding_name binding;
                    value =
                      JsAst.Field
                        { obj = JsAst.Var used_var; field = binding.name.name };
                  }))
      | E_If _ -> [ Expr (transpile_expr expr) ]
      | E_And _ -> failwith __LOC__
      | E_Or _ -> failwith __LOC__
      | E_Match _ -> [ Expr (transpile_expr expr) ]
      | E_QuoteAst _ -> failwith __LOC__
      | E_Loop { body } ->
          [
            For
              {
                init = None;
                cond = None;
                after = None;
                body = transpile_expr_as_stmts body;
              };
          ]
      | E_Unwindable _ -> [ Expr (transpile_expr expr) ]
      | E_Unwind { token; value } ->
          [
            Throw
              (Obj
                 [
                   Field { name = "unwind_token"; value = transpile_expr token };
                   Field { name = "value"; value = transpile_expr value };
                 ]);
          ]
      | E_InjectContext _ -> failwith __LOC__
      | E_CurrentContext _ -> failwith __LOC__
      | E_ImplCast _ -> failwith __LOC__
      | E_Cast _ -> failwith __LOC__
      | E_TargetDependent _ -> failwith __LOC__
      | E_Error -> failwith __LOC__
    with e ->
      Log.error (fun log ->
          log "While transpiling expr as stmts %a" Span.print expr.data.span);
      raise e

  and prepend stmts =
    let ctx = Effect.perform GetCtx in
    ctx.mut.prepend <- ctx.mut.prepend @ stmts

  and impl_cast ~value ~target ~impl : JsAst.stmt list =
    [
      Expr
        (Call
           {
             async = false;
             f = Raw "Kast.casts.add_impl";
             args =
               [
                 Obj
                   [
                     Field { name = "value"; value };
                     Field { name = "target"; value = target };
                     Field { name = "impl"; value = impl };
                   ];
               ];
           });
    ]

  and impl_as_module ~value ~impl ~ty : JsAst.stmt list =
    let ty =
      ty |> Ty.await_inferred |> Ty.Shape.expect_tuple
      |> Option.unwrap_or_else (fun () -> fail "impl as module not tuple??")
    in
    let value_var = JsAst.gen_name "value" in
    let impl_var = JsAst.gen_name "impl" in
    [
      JsAst.Let { var = value_var; value };
      JsAst.Let { var = impl_var; value = impl };
    ]
    @ (ty.tuple |> Tuple.to_seq
      |> Seq.map (fun (member, _) ->
          let field = tuple_field_name member in
          JsAst.Assign
            {
              assignee = Field { obj = Var value_var; field };
              value = Field { obj = Var impl_var; field };
            })
      |> List.of_seq)

  and symbol_for (label : Label.t) : JsAst.name =
    let id = (Label.get_data label).id in
    let ctx = Effect.perform GetCtx in
    match ctx.mut.symbols |> Id.Map.find_opt id with
    | None ->
        let name = JsAst.gen_name "symbol" in
        prepend
          [
            Let
              {
                var = name;
                value = Raw (make_string "Symbol(%S)" (Label.get_name label));
              };
          ];
        ctx.mut.symbols <- ctx.mut.symbols |> Id.Map.add id name;
        name
    | Some name -> name

  and raise_error message : JsAst.stmt =
    Throw (Call { async = false; f = Raw "Error"; args = [ String message ] })

  and transpile_expr : expr -> JsAst.expr =
   fun expr ->
    try
      let ctx = Effect.perform GetCtx in
      match expr.shape with
      | E_Constant value -> transpile_value value
      | E_Ref { mut; place } -> transpile_place place
      | E_Claim expr -> transpile_place expr |> claim
      | E_Then { list } ->
          let body =
            list
            |> List.mapi (fun i e ->
                if i + 1 = List.length list then
                  [ JsAst.Return (transpile_expr e) ]
                else transpile_expr_as_stmts e)
            |> List.flatten
          in
          JsAst.Call
            {
              async = true;
              f = JsAst.Fn { async = true; args = []; body };
              args = [];
            }
      | E_Stmt { expr } -> transpile_expr expr
      | E_Scope { expr } ->
          (* TODO should I actually create js scope? *)
          transpile_expr expr
      | E_Fn { ty = _; def } -> fn ~captured:None def
      | E_Generic { def; ty = _ } -> fn ~captured:None def
      | E_Tuple { parts } ->
          let idx = ref 0 in
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
                      {
                        name = tuple_field_name member;
                        value = transpile_expr value;
                      }
                | Unpack packed -> failwith __LOC__))
      | E_Variant { label; label_span = _; value } ->
          JsAst.Obj
            [
              Field { name = "tag"; value = JsAst.Var (symbol_for label) };
              Field
                {
                  name = "data";
                  value =
                    (match value with
                    | Some value -> transpile_expr value
                    | None -> JsAst.Undefined);
                };
            ]
      | E_Apply { f; arg } ->
          JsAst.Call
            {
              async = true;
              f = transpile_expr f;
              args = [ transpile_expr arg ];
            }
      | E_InstantiateGeneric { generic; arg } ->
          JsAst.Call
            {
              async = true;
              f = transpile_expr generic;
              args = [ transpile_expr arg ];
            }
      | E_Assign _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_Ty _ -> JsAst.Null
      | E_Newtype _ -> JsAst.Null
      | E_Native { id = _; expr } -> JsAst.Raw expr
      | E_Module { def; bindings } ->
          let module_value =
            JsAst.Obj
              (bindings
              |> List.map (fun binding ->
                  JsAst.Field
                    {
                      name = binding.name.name;
                      value = Var (binding_name binding);
                    }))
          in
          scope (transpile_expr_as_stmts def @ [ Return module_value ])
      | E_UseDotStar _ -> failwith __LOC__
      | E_If { cond; then_case; else_case } ->
          scope
            [
              If
                {
                  condition = transpile_expr cond;
                  then_case = [ Return (transpile_expr then_case) ];
                  else_case = Some [ Return (transpile_expr else_case) ];
                };
            ]
      | E_And { lhs; rhs } ->
          BinOp { op = And; lhs = transpile_expr lhs; rhs = transpile_expr rhs }
      | E_Or { lhs; rhs } ->
          BinOp { op = Or; lhs = transpile_expr lhs; rhs = transpile_expr rhs }
      | E_Match { value; branches } ->
          scope
            (let var = JsAst.gen_name "matched" in
             let stmts =
               ref [ JsAst.Let { var; value = transpile_place value } ]
             in
             stmts :=
               !stmts
               @ (branches
                 |> List.map (fun { pattern; body } ->
                     JsAst.If
                       {
                         condition = does_match pattern (Var var);
                         then_case =
                           pattern_match pattern (Var var)
                           @ [ JsAst.Return (transpile_expr body) ];
                         else_case = None;
                       }));
             stmts := !stmts @ [ raise_error "pattern match non exhaustive" ];
             !stmts)
      | E_QuoteAst _ -> failwith __LOC__
      | E_Loop _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_Unwindable { token; body } ->
          let token_var = JsAst.gen_name "unwind_token" in
          let catch_var = JsAst.gen_name "e" in
          scope
            [
              Let { var = token_var; value = Raw "Symbol()" };
              Try
                {
                  body =
                    pattern_match token (place_of (Var token_var))
                    @ transpile_expr_as_stmts body;
                  catch_var;
                  catch_body =
                    [
                      If
                        {
                          condition =
                            Compare
                              {
                                op = Equal;
                                lhs =
                                  Field
                                    {
                                      obj = Var catch_var;
                                      field = "unwind_token";
                                    };
                                rhs = Var token_var;
                              };
                          then_case =
                            [
                              Return
                                (Field { obj = Var catch_var; field = "value" });
                            ];
                          else_case = None;
                        };
                      Throw (Var catch_var);
                    ];
                };
            ]
      | E_Unwind _ -> transpile_expr_as_stmts expr |> stmts_expr
      | E_InjectContext _ -> failwith __LOC__
      | E_CurrentContext _ -> failwith __LOC__
      | E_ImplCast _ -> failwith __LOC__
      | E_Cast { value; target } ->
          let value = transpile_expr value in
          let target = transpile_value target in
          JsAst.Call
            {
              async = false;
              f = JsAst.Raw "Kast.casts.get_impl";
              args =
                [
                  Obj
                    [
                      Field { name = "value"; value };
                      Field { name = "target"; value = target };
                    ];
                ];
            }
      | E_TargetDependent target_dependent -> (
          let branch =
            Kast_interpreter.find_target_dependent_branch ctx.interpreter
              target_dependent ctx.target
          in
          match branch with
          | Some branch -> transpile_expr branch.body
          | None -> todo_expr "no js cfg branch at %a" Span.print expr.data.span
          )
      | E_Error -> failwith __LOC__
    with e ->
      Log.error (fun log ->
          log "While transpiling expr %a" Span.print expr.data.span);
      raise e
end

type result = { print : formatter -> unit }

let with_ctx ~state ~span f =
  let ctx : ctx =
    {
      interpreter = state;
      captured = state.scope;
      mut = { values = ValueMap.empty; symbols = Id.Map.empty; prepend = [] };
      span;
      target = { name = "javascript" };
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
    with effect GetCtx, k -> Effect.continue k ctx
  in
  let result_var = JsAst.gen_name "result" in
  let ast : JsAst.expr =
    Impl.scope
      ([ JsAst.Raw [%blob "./runtime.js"] ]
      @ ctx.mut.prepend
      @ [
          Let { var = result_var; value = result };
          Raw "Kast.cleanup()";
          Return (Var result_var);
        ])
  in
  let ast : JsAst.expr =
    match ast with
    | Call { async = true; f; args } -> Call { async = false; f; args }
    | _ -> failwith __LOC__
  in
  { print = (fun fmt -> JsAst.print_expr ~precedence:None fmt ast) }

let transpile_value : state:interpreter_state -> span:span -> value -> result =
 fun ~state ~span value ->
  with_ctx ~state ~span (fun () -> Impl.transpile_value value)

let transpile_expr : state:interpreter_state -> span:span -> expr -> result =
 fun ~state ~span expr ->
  with_ctx ~state ~span (fun () -> Impl.transpile_expr expr)
