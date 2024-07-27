open Prelude
open Types

module Make
    (Interpreter : Modules.Interpreter)
    (Inference : Modules.Inference)
    (Show : Modules.Show)
    (Utils : Modules.Utils)
    (TypeId : Modules.TypeId) : Modules.Compiler = struct
  open Show
  open Interpreter
  open Utils

  let rec type_of_value (value : value) ~(ensure : bool) : value_type =
    match value with
    | Binding { value_type; _ } -> InferVar value_type
    | InferVar var ->
        let result =
          match Inference.get_inferred var with
          | Some (InferVar inner : value)
            when Inference.get_id inner = Inference.get_id var ->
              failwith "wtf var = var"
          | Some value -> type_of_value value ~ensure
          | None -> Inference.get_type var
        in
        result
    | Var { typ; _ } -> typ
    | Ast _ -> Ast
    | UnwindToken _ -> UnwindToken
    | DelimitedToken _ -> DelimitedToken
    | Void -> Void
    | Variant { typ; _ } -> typ
    | BuiltinMacro _ -> BuiltinMacro
    | BuiltinFn _ -> Fn (fn_type_vars_to_type @@ new_fn_type_vars ())
    | Template f -> Template (template_to_template_type f)
    | Macro f -> Macro (type_of_fn ~ensure f)
    | Function f -> Fn (type_of_fn ~ensure f)
    | Int32 _ -> Int32
    | Int64 _ -> Int64
    | Float64 _ -> Float64
    | Bool _ -> Bool
    | String _ -> String
    | Dict { fields } ->
        Dict { fields = StringMap.map (type_of_value ~ensure) fields }
    | Ref _ -> failwith "todo ref"
    | Struct { data; _ } ->
        Dict
          {
            fields =
              StringMap.map
                (fun ({ value; _ } : local) -> type_of_value ~ensure value)
                data.locals;
          }
    | Type t -> Type

  and set_ir_type (ir : ir) (t : value_type) =
    Inference.set (ir_data ir).type_var (Type t : value)

  and new_fn_type_vars () : fn_type_vars =
    {
      arg_type = Inference.new_var ();
      result_type = Inference.new_var ();
      contexts = Inference.new_var ();
    }

  and fn_type_vars_to_type ({ arg_type; result_type; contexts } : fn_type_vars)
      : fn_type =
    {
      arg_type = InferVar arg_type;
      result_type = InferVar result_type;
      contexts = Interpreter.empty_contexts_type (* TODO InferVar contexts *);
    }

  and var_type (var : inference_var) : value_type =
    match Inference.get_inferred var with
    | Some (Type t : value) -> t
    | Some value -> failwith @@ "ir inferred to be not a type but " ^ show value
    | None -> InferVar var

  and pattern_type (pattern : pattern) : value_type =
    var_type (pattern_data pattern).type_var

  and ir_type (ir : ir) : value_type = var_type (ir_data ir).type_var

  and set_pattern_type (pattern : pattern) (t : value_type) =
    Inference.set (pattern_data pattern).type_var (Type t : value)

  and type_into_var t : inference_var =
    let var = Inference.new_var () in
    Inference.set var (Type t : value);
    var

  and contexts_type_into_var contexts =
    let var = Inference.new_var () in
    (* TODO *)
    var

  and inferred_type var =
    match (Inference.get_inferred var : value option) with
    | Some (Type t) -> t
    | Some _ -> failwith "inferred value expected to be a type"
    | None -> InferVar var

  and init_ir (ir : no_data ir_node) : ir =
    (* Log.trace @@ "initializing ir: " ^ show_ir_with_data (fun _ -> None) ir; *)
    Log.trace @@ "initializing " ^ ir_name ir;
    try
      let known value =
        let type_var = Inference.new_var () in
        Inference.set type_var value;
        { type_var }
      in
      let known_type t = known (Type t : value) in
      let unknown () = { type_var = Inference.new_var () } in
      let same_as (other : ir) = { type_var = (ir_data other).type_var } in
      let result : ir =
        match ir with
        | Void { data = NoData } -> Void { data = known_type Void }
        | Const { value; data = NoData } ->
            Const
              { value; data = known_type @@ type_of_value ~ensure:false value }
        | Struct { body; data = NoData } -> Struct { body; data = same_as body }
        | Assign { pattern; value; data = NoData } ->
            Inference.make_same (pattern_data pattern).type_var
              (ir_data value).type_var;
            Assign { pattern; value; data = known_type Void }
        | CreateImpl
            {
              captured;
              value = value_ir;
              trait = trait_ir;
              impl;
              data = NoData;
            } ->
            let trait = (eval_ir captured trait_ir).value in
            let value = (eval_ir captured value_ir).value in
            (match trait with
            | Type t -> set_ir_type impl @@ t
            | Template t ->
                let result =
                  call_compiled empty_contexts (ensure_compiled t) value
                  |> value_to_type
                in
                set_ir_type impl result
            | _ ->
                Log.error @@ show trait ^ " can not be treated as trait";
                failwith "not a trait");
            CreateImpl
              {
                captured;
                value = value_ir;
                trait = trait_ir;
                impl;
                data = known_type Void;
              }
        | GetImpl
            { captured; value = value_ir; trait = trait_ir; data = NoData } -> (
            let trait = (eval_ir captured trait_ir).value in
            Log.trace @@ "trait = " ^ show trait;
            match trait with
            | Type t ->
                GetImpl
                  {
                    captured;
                    value = value_ir;
                    trait = trait_ir;
                    data = known_type t;
                  }
            | Template t ->
                let value = (eval_ir captured value_ir).value in
                let t =
                  call_compiled empty_contexts (ensure_compiled t) value
                  |> value_to_type
                in
                Log.trace @@ show_type t;
                GetImpl
                  {
                    captured;
                    value = value_ir;
                    trait = trait_ir;
                    data = known_type t;
                  }
            | _ ->
                Log.error @@ show trait ^ " can not be treated as trait";
                failwith "not a trait"
            (* set_ir_type ty Type; *))
        | CheckImpl { captured; value; trait = trait_ir; data = NoData } ->
            (* let trait = eval_ir captured trait_ir in *)
            CheckImpl
              { captured; value; trait = trait_ir; data = known_type Bool }
        | Match { value; branches; data = NoData } ->
            let value_var = (ir_data value).type_var in
            let result_type_var = Inference.new_var () in
            List.iter
              (fun { pattern; body } ->
                Inference.make_same result_type_var (ir_data body).type_var;
                Inference.make_same value_var (pattern_data pattern).type_var)
              branches;
            Match { value; branches; data = { type_var = result_type_var } }
        | NewType { def; data = NoData } ->
            set_pattern_type def Type;
            NewType { def; data = known_type Type }
        | Scope { expr; data = NoData } -> Scope { expr; data = same_as expr }
        | Number { raw; data = NoData } -> Number { raw; data = unknown () }
        | String { data = NoData; raw; value } ->
            String { raw; value; data = known_type String }
        | TypeOf { data = NoData; captured; expr } ->
            TypeOf { data = known_type Type; captured; expr }
        | TypeOfValue { data = NoData; captured; expr } ->
            TypeOfValue { data = known_type Type; captured; expr }
        | ConstructVariant { data = NoData; ty; variant; value } ->
            (match ty with
            | OneOf variants -> (
                let variant = StringMap.find variant variants in
                match (variant, value) with
                | Some value_ty, Some value ->
                    Log.trace @@ "setting type of " ^ show_ir value ^ " = "
                    ^ show_type value_ty;
                    set_ir_type value value_ty
                | Some _, None ->
                    failwith "variant expected a value, but value not provided"
                | None, Some _ -> failwith "variant didnt expect a value"
                | None, None -> ())
            | _ -> failwith @@ show_type ty ^ " is not a oneof");
            ConstructVariant { data = known_type @@ ty; ty; variant; value }
        | OneOf { data = NoData; variants } ->
            OneOf { data = known_type Type; variants }
        | Let { data = NoData; pattern; value } ->
            Inference.make_same (pattern_data pattern).type_var
              (ir_data value).type_var;
            Let { data = known_type Void; pattern; value }
        | Discard { data = NoData; value } ->
            Discard { data = known_type Void; value }
        | If { data = NoData; cond; then_case; else_case } ->
            set_ir_type cond Bool;
            Inference.make_same (ir_data then_case).type_var
              (ir_data else_case).type_var;
            If { data = same_as else_case; cond; then_case; else_case }
        | Ast { data = NoData; def; ast_data; values } ->
            values |> StringMap.iter (fun _name value -> set_ir_type value Ast);
            Ast { data = known_type Ast; def; ast_data; values }
        | Dict { data = NoData; fields } ->
            Dict
              {
                data =
                  known_type
                  @@ Dict
                       {
                         fields =
                           fields |> StringMap.map (fun field -> ir_type field);
                       };
                fields;
              }
        | UnwindableBlock { data = NoData; f } ->
            let f_type = new_fn_type_vars () in
            Inference.set f_type.arg_type (Type UnwindToken : value);
            set_ir_type f @@ Fn (fn_type_vars_to_type f_type);
            UnwindableBlock { data = { type_var = f_type.result_type }; f }
        | Call { data = NoData; f; args } -> (
            match (ir_data f).type_var |> Inference.get_inferred with
            | Some (Type (Template t) : value) ->
                Log.trace "auto instantiating template";
                let instantiated =
                  Instantiate
                    {
                      data = NoData;
                      captured = t.captured;
                      template = f;
                      args =
                        Const
                          {
                            data = NoData;
                            value = InferVar (Inference.new_var ());
                          }
                        |> init_ir;
                    }
                  |> init_ir
                in
                Call { data = NoData; f = instantiated; args } |> init_ir
            | _ ->
                Log.trace "calling an actual fn";
                let f_type = new_fn_type_vars () in
                Inference.make_same f_type.arg_type (ir_data args).type_var;
                set_ir_type f @@ Fn (fn_type_vars_to_type f_type);
                Call { data = { type_var = f_type.result_type }; f; args })
        | Then { data = NoData; first; second } ->
            set_ir_type first Void;
            Then { data = same_as second; first; second }
        | Binding { data = NoData; binding } ->
            Binding { data = { type_var = binding.value_type }; binding }
        | Function { data = NoData; f } ->
            Function
              { data = known_type @@ Fn (fn_type_vars_to_type f.vars); f }
        | Template { data = NoData; f } ->
            Template
              { data = known_type @@ Template (template_to_template_type f); f }
        | WithContext { data = NoData; new_context; expr } ->
            WithContext { data = same_as expr; new_context; expr }
        | CurrentContext { data = NoData; context_type } ->
            CurrentContext { data = known_type context_type; context_type }
        | FieldAccess { data = NoData; obj; name; default_value } ->
            let field_type_var = Inference.new_var () in
            Inference.add_check (ir_data obj).type_var (fun value ->
                Log.trace @@ "checking field " ^ name ^ " of " ^ show value;
                match value with
                | Type Type -> failwith "todo"
                | _ -> (
                    match get_field_opt value name with
                    | None ->
                        failwith @@ "inferred type doesnt have field " ^ name
                    | Some field ->
                        Inference.set field_type_var field;
                        true));
            (* todo
               Inference.expand_dict obj.var name?
            *)
            (match default_value with
            | Some default ->
                Inference.make_same field_type_var (ir_data default).type_var
            | None -> ());
            FieldAccess
              { data = { type_var = field_type_var }; obj; name; default_value }
        | BuiltinFn { data = NoData; f } ->
            BuiltinFn
              {
                (* because multitarget *)
                data =
                  known_type @@ Fn (new_fn_type_vars () |> fn_type_vars_to_type);
                f;
              }
        | Instantiate { data = NoData; captured; template; args = args_ir } -> (
            match (eval_ir captured template).value with
            | Template t ->
                (*
                let compiled = ensure_compiled t in
                Log.info @@ "template :: "
                ^ show_fn_type (fn_type_vars_to_type t.vars);
                Log.info @@ "compiled result type = "
                ^ show_type compiled.result_type; *)
                (* TODO without template_to_template_type ?? *)
                let tt = template_to_template_type t in
                let compiled_tt = ensure_compiled tt in
                let args = (eval_ir captured args_ir).value in
                let result_type =
                  call_compiled empty_contexts compiled_tt args
                in
                Log.trace @@ "instantiate result type = " ^ show result_type;
                Instantiate
                  {
                    data = known_type @@ value_to_type result_type;
                    captured = t.captured;
                    template;
                    args = args_ir;
                  }
            | other -> failwith @@ show other ^ " is not a template")
      in
      (* Log.trace @@ "almost: " ^ ir_name result; *)
      Log.trace @@ "initialized ir: " ^ show_ir result;
      result
    with Failure _ as failure ->
      Log.error @@ "  while initializing ir " ^ ir_name ir;
      raise failure

  and init_pattern (p : no_data pattern_node) : pattern =
    let known value =
      let type_var = Inference.new_var () in
      Inference.set type_var value;
      { type_var }
    in
    let known_type t = known (Type t : value) in
    let unknown () = { type_var = Inference.new_var () } in
    let same_as other = { type_var = (pattern_data other).type_var } in
    match p with
    | Placeholder { data = NoData } -> Placeholder { data = unknown () }
    | Void { data = NoData } -> Void { data = known_type Void }
    | Binding { data = NoData; binding } ->
        Binding { data = { type_var = binding.value_type }; binding }
    | Dict { data = NoData; fields } ->
        Dict
          {
            data =
              known_type
              @@ Dict { fields = fields |> StringMap.map pattern_type };
            fields;
          }
    | Variant { data = NoData; name; value } ->
        (* TODO Inference.expand_sum_type with variant name *)
        Variant { data = unknown (); name; value }
    | Union { data = NoData; a; b } ->
        Inference.make_same (pattern_data a).type_var (pattern_data b).type_var;
        Union { data = same_as a; a; b }

  and type_of_fn (f : fn) ~(ensure : bool) : fn_type =
    if ensure then (
      Log.trace "getting type of fun";
      ignore @@ ensure_compiled f);
    match f.compiled with
    | None ->
        {
          result_type = InferVar f.vars.result_type;
          arg_type = InferVar f.vars.arg_type;
          contexts =
            empty_contexts_type
            (* TODO value_to_contexts_type (InferVar f.vars.contexts) *);
        }
    | Some compiled ->
        {
          result_type = compiled.result_type;
          arg_type = inferred_type (pattern_data compiled.args).type_var;
          contexts = compiled.contexts;
        }

  and template_to_template_type (f : fn) : fn =
    match f.cached_template_type with
    | Some t -> t
    | None -> (
        try
          let t =
            {
              id = Id.gen ();
              cached_template_type = None;
              vars =
                {
                  arg_type = f.vars.arg_type;
                  contexts = f.vars.contexts;
                  result_type = Inference.new_var ();
                };
              shared_compiled_args = f.shared_compiled_args;
              ast =
                {
                  args = f.ast.args;
                  contexts = f.ast.contexts;
                  where = f.ast.where;
                  returns = f.ast.returns;
                  body =
                    Complex
                      {
                        def =
                          {
                            (* TODO how to not hardcode this here? *)
                            name = "builtin_macro_typeof";
                            assoc = Left;
                            priority = 0.0;
                            parts = [ Keyword "typeof"; Binding "expr" ];
                          };
                        values = StringMap.singleton "expr" f.ast.body;
                        data = Ast.data f.ast.body;
                      };
                };
              captured = f.captured;
              compiled = None;
            }
          in
          f.cached_template_type <- Some t;
          t
        with Failure _ as failure ->
          Log.error @@ "  while template_to_template_type";
          raise failure)

  and compile_ast_to_ir (self : state) (ast : ast) : compiled =
    (* Log.trace @@ "compiling ast to ir: " ^ Ast.show ast; *)
    let result =
      try
        match ast with
        | Nothing _ ->
            {
              ir = Void { data = NoData } |> init_ir;
              new_bindings = StringMap.empty;
            }
        | Simple { token; _ } ->
            {
              ir =
                (match token with
                | Ident ident -> (
                    match get_local_opt self ident with
                    | None ->
                        log_state Log.Info self;
                        failwith (ident ^ " not found in current scope")
                    | Some local ->
                        Binding { binding = local.binding; data = NoData }
                        |> init_ir)
                | Number raw -> Number { raw; data = NoData } |> init_ir
                | String { value; raw } ->
                    String { value; raw; data = NoData } |> init_ir
                | Punctuation _ -> failwith "punctuation");
              new_bindings = StringMap.empty;
            }
        | Complex { def; values; _ } -> (
            match expand_macro self def.name values ~new_bindings:false with
            | Compiled result -> result
            | Pattern _ ->
                Log.error @@ Ast.show ast;
                failwith
                  "wtf ast was compiled to a pattern but expected compiled")
        | Syntax { def; value; _ } -> compile_ast_to_ir self value
      with Failure _ as failure ->
        Log.error @@ "  while compiling to ir: " ^ Ast.name ast ^ " at "
        ^ Span.show (Ast.data ast).span;
        raise failure
    in
    (* Log.info @@ "compiled ast: " ^ Ast.show ast ^ " as " ^ show_ir result.ir; *)
    result

  and compile_pattern (self : state) (pattern : ast option) : pattern =
    try
      match pattern with
      | None -> Void { data = NoData } |> init_pattern
      | Some ast -> (
          Log.trace @@ "compiling pattern " ^ Ast.show ast;
          match ast with
          | Nothing _ -> Void { data = NoData } |> init_pattern
          | Simple { token; _ } -> (
              match token with
              | Ident ident ->
                  Binding
                    {
                      binding =
                        {
                          id = Id.gen ();
                          name = ident;
                          value_type = Inference.new_var ();
                          mut = false;
                        };
                      data = NoData;
                    }
                  |> init_pattern
              | Number raw -> failwith "todo pattern number"
              | String { value; raw } -> failwith "todo string pattern"
              | Punctuation _ -> failwith "punctuation")
          | Complex { def; values; _ } -> (
              match expand_macro self def.name values ~new_bindings:true with
              | Pattern result -> result
              | Compiled _ -> failwith @@ "wtf " ^ Ast.show ast)
          | Syntax { def; value; _ } ->
              failwith "syntax definition inlined in a pattern wtf?")
    with Failure _ as failure ->
      Log.error @@ "  while compiling pattern "
      ^ show_or "<empty>" Ast.show pattern;
      raise failure

  and pattern_bindings (pattern : pattern) : binding StringMap.t =
    match pattern with
    | Placeholder _ -> StringMap.empty
    | Void _ -> StringMap.empty
    | Variant { value; _ } -> (
        match value with
        | Some value -> pattern_bindings value
        | None -> StringMap.empty)
    | Binding { binding; _ } -> StringMap.singleton binding.name binding
    | Union { a; b; _ } ->
        let a = pattern_bindings a in
        let b = pattern_bindings b in
        if not (StringMap.equal (fun (a : binding) b -> a.name = b.name) a b)
        then failwith "pattern variants have different bindings";
        a
    | Dict { fields; _ } ->
        StringMap.fold
          (fun _name field acc ->
            StringMap.union
              (fun name _old _new ->
                failwith (name ^ " appears multiple times in pattern"))
              acc (pattern_bindings field))
          fields StringMap.empty

  and expand_ast (self : state) (ast : ast) ~(new_bindings : bool) :
      expanded_macro =
    match new_bindings with
    | true -> Pattern (compile_pattern self (Some ast))
    | false -> Compiled (compile_ast_to_ir self ast)

  and expand_macro (self : state) (name : string) (values : ast StringMap.t)
      ~(new_bindings : bool) : expanded_macro =
    match get_local_value_opt self name with
    | None -> failwith (name ^ " not found")
    | Some value -> (
        match value with
        | Function _ | BuiltinFn _ | Template _ ->
            let args : ir =
              Dict
                {
                  fields =
                    StringMap.map
                      (fun arg -> (compile_ast_to_ir self arg).ir)
                      values;
                  data = NoData;
                }
              |> init_ir
            in
            Compiled
              {
                ir =
                  Call
                    {
                      f = Const { value; data = NoData } |> init_ir;
                      args;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              }
        | BuiltinMacro { impl; _ } -> impl self values ~new_bindings
        | Macro f -> (
            let values : value =
              Dict { fields = StringMap.map (fun x : value -> Ast x) values }
            in
            Log.trace ("call macro with " ^ show values);
            let compiled = ensure_compiled f in
            match call_compiled empty_contexts compiled values with
            | Ast new_ast ->
                Log.trace ("macro expanded to " ^ Ast.show new_ast);
                expand_ast self new_ast ~new_bindings
            | _ -> failwith "macro returned not an ast")
        | _ -> failwith (name ^ " is not a macro"))

  and ensure_compiled (f : fn) : compiled_fn =
    if Option.is_none f.compiled then (
      Log.trace ("compiling " ^ show_fn_ast f.ast);
      let compiled =
        try
          let args =
            match !(f.shared_compiled_args) with
            | Some args -> args
            | None ->
                let args = compile_pattern f.captured f.ast.args in
                f.shared_compiled_args := Some args;
                args
          in

          (try
             match f.ast.returns with
             | Some ast ->
                 Inference.set f.vars.result_type
                   (eval_ast f.captured ast).value
             | None -> ()
           with Failure _ as failure ->
             Log.error @@ "  while result type inference";
             raise failure);
          (try Inference.make_same (pattern_data args).type_var f.vars.arg_type
           with Failure _ as failure ->
             Log.error @@ "  while args inference";
             raise failure);
          let captured_with_args_bindings =
            {
              f.captured with
              data =
                {
                  f.captured.data with
                  locals =
                    update_locals f.captured.data.locals
                      (StringMap.map
                         (fun binding : local ->
                           { value = Binding binding; binding })
                         (pattern_bindings args));
                };
            }
          in
          let body =
            (compile_ast_to_ir captured_with_args_bindings f.ast.body).ir
          in
          Inference.make_same (ir_data body).type_var f.vars.result_type;
          {
            captured = f.captured;
            where_clause =
              (match f.ast.where with
              | None -> Const { value = Bool true; data = NoData } |> init_ir
              | Some clause ->
                  (compile_ast_to_ir captured_with_args_bindings clause).ir);
            args;
            result_type = InferVar f.vars.result_type;
            result_type_ir =
              (match f.ast.returns with
              | None -> None
              | Some ast ->
                  Some (compile_ast_to_ir captured_with_args_bindings ast).ir);
            body;
            contexts =
              (match f.ast.contexts with
              | Some contexts ->
                  value_to_contexts_type (eval_ast f.captured contexts).value
              | None -> default_contexts_type);
          }
        with Failure _ as failure ->
          Log.error @@ "  while compiling " ^ show_fn_ast f.ast;
          raise failure
      in
      f.compiled <- Some compiled;
      Log.trace @@ "compiled as " ^ show_fn_type (type_of_fn ~ensure:false f));
    Option.get f.compiled

  and value_to_contexts_type : value -> contexts_type =
   fun value ->
    let context_type = value_to_type value in
    Id.Map.singleton (TypeId.get context_type) 1
end
