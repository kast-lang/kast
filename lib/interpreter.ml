open Prelude

module type Interpreter = sig
  type value
  type value_type
  type state

  val empty : unit -> state
  val inference_unite : value -> value -> value
  val eval : state ref -> string -> filename:string -> value
  val show : value -> string
  val show_type : value_type -> string
  val type_of_value : value -> ensure:bool -> value_type
  val eval_file : state ref -> string -> value
  val discard : value -> unit
end

module rec Impl : Interpreter = struct
  type ast_data = { span : Span.span }
  type ast = ast_data Ast.node
  type id = Id.t

  type value =
    | Binding of binding
    | Var of { id : id; typ : value_type }
    | InferVar of inference_var
    | UnwindToken of id
    | Ast of ast
    | Macro of fn
    | BuiltinMacro of builtin_macro
    | BuiltinFn of builtin_fn
    | Template of fn
    | Function of fn
    | Void
    | Bool of bool
    | Int32 of int32
    | Int64 of int64
    | Float64 of float
    | String of string
    | Dict of { fields : value StringMap.t }
    | Struct of struct'
    | Ref of value ref
    | Type of value_type
    | Variant of { typ : value_type; name : string; value : value option }

  and value_type =
    | Var of { id : id }
    | Binding of binding
    | UnwindToken
    | Never
    | Ast
    | Void
    | Bool
    | Int32
    | Int64
    | Float32
    | Float64
    | String
    | Fn of fn_type
    | Macro of fn_type
    | Template of fn
    | BuiltinMacro
    | Dict of { fields : value_type StringMap.t }
    | NewType of value_type
    | OneOf of value_type option StringMap.t
    | Union of (value_type, unit) Hashtbl.t
    | Type
    | InferVar of MyInference.var
    | MultiSet of (value_type, int) Hashtbl.t

  and inference_var = MyInference.var
  and type_var_map = value_type Id.Map.t

  and fn_type = {
    arg_type : value_type;
    contexts : contexts_type;
    result_type : value_type;
  }

  and fn_type_vars = {
    arg_type : inference_var;
    contexts : inference_var;
    result_type : inference_var;
  }

  and builtin_fn = { name : string; impl : value -> value }

  and builtin_macro = {
    name : string;
    impl : state -> ast StringMap.t -> new_bindings:bool -> expanded_macro;
  }

  and 'data match_branch = { pattern : pattern; body : ir }

  and 'data get_impl = {
    captured : state;
    value : ir;
    trait : ir;
    data : 'data;
  }

  and 'data ir_node =
    | Void of { data : 'data }
    | Struct of { body : ir; data : 'data }
    | CreateImpl of {
        captured : state;
        value : ir;
        trait : ir;
        impl : ir;
        data : 'data;
      }
    | GetImpl of 'data get_impl
    | CheckImpl of 'data get_impl
    | Match of {
        value : ir;
        branches : ir_data match_branch list;
        data : 'data;
      }
    | NewType of { def : pattern; data : 'data }
    | Scope of { expr : ir; data : 'data }
    | OneOf of { variants : ir option StringMap.t; data : 'data }
    | TypeOf of { captured : state; expr : ir; data : 'data }
    | TypeOfValue of { captured : state; expr : ir; data : 'data }
    | Dict of { fields : ir StringMap.t; data : 'data }
    | Unwinding of { f : ir; data : 'data }
    | WithContext of { new_context : ir; expr : ir; data : 'data }
    | CurrentContext of { context_type : value_type; data : 'data }
    | Ast of {
        def : Syntax.syntax_def;
        ast_data : ast_data;
        values : ir StringMap.t;
        data : 'data;
      }
    | Template of { f : fn; data : 'data }
    | Function of { f : fn; data : 'data }
    | FieldAccess of {
        obj : ir;
        name : string;
        default_value : ir option;
        data : 'data;
      }
    | Const of { value : value; data : 'data }
    | Binding of { binding : binding; data : 'data }
    | Number of { raw : string; data : 'data }
    | String of { raw : string; value : string; data : 'data }
    | Discard of { value : ir; data : 'data }
    | Then of { first : ir; second : ir; data : 'data }
    | Call of { f : ir; args : ir; data : 'data }
    | Instantiate of {
        (* todo remove? *)
        captured : state;
        template : ir;
        args : ir;
        data : 'data;
      }
    | BuiltinFn of { f : builtin_fn; data : 'data }
    | If of { cond : ir; then_case : ir; else_case : ir; data : 'data }
    | Let of { pattern : pattern; value : ir; data : 'data }

  and inference_status = NotYet | InProgress | Done
  and type_inference_data = { type_var : inference_var }
  and no_data = NoData
  and ir_data = type_inference_data
  and ir = ir_data ir_node

  and fn_ast = {
    where : ast option;
    args : ast option;
    returns : ast option;
    contexts : ast option;
    body : ast;
  }

  and fn = {
    id : Id.t;
    mutable cached_template_type : fn option;
    captured : state;
    ast : fn_ast;
    vars : fn_type_vars;
    mutable compiled : compiled_fn option;
  }

  and compiled_fn = {
    where_clause : ir;
    result_type_ir : ir option;
    result_type : value_type;
    contexts : contexts_type;
    captured : state;
    args : pattern;
    body : ir;
  }

  and 'data pattern_node =
    | Placeholder of { data : 'data }
    | Void of { data : 'data }
    | Binding of { binding : binding; data : 'data }
    | Dict of { fields : pattern StringMap.t; data : 'data }
    | Variant of { name : string; value : pattern option; data : 'data }
    | Union of { a : pattern; b : pattern; data : 'data }

  and pattern_data = type_inference_data
  and pattern = pattern_data pattern_node

  (* todo GADT? *)
  and expanded_macro = Compiled of compiled | Pattern of pattern
  and evaled = { value : value; new_bindings : value StringMap.t }
  and compiled = { ir : ir; new_bindings : value StringMap.t }
  and struct' = { parent : struct' option; mutable data : state_data }
  and contexts = (value_type, value list) Hashtbl.t
  and contexts_type = (value_type, int) Hashtbl.t
  and state = { self : struct'; data : state_data; contexts : contexts }
  and state_data = { locals : value StringMap.t; syntax : Syntax.syntax }
  and binding = { id : id; name : string; value_type : inference_var }

  exception Unwind of id * value

  let empty_contexts : contexts = Hashtbl.create 0
  let empty_contexts_type : contexts_type = Hashtbl.create 0
  let default_contexts_type : contexts_type = empty_contexts_type

  (* type -> trait -> value *)
  let trait_impls : (id, (id, value) Hashtbl.t) Hashtbl.t = Hashtbl.create 0
  let empty_type_var_map () : type_var_map ref = ref Id.Map.empty
  let failinfer () = raise @@ Inference.FailedUnite "inference union failed"

  module TypeIds = struct
    let unwind_token = Id.gen ()
    let never = Id.gen ()
    let ast = Id.gen ()
    let void = Id.gen ()
    let bool = Id.gen ()
    let int32 = Id.gen ()
    let int64 = Id.gen ()
    let float32 = Id.gen ()
    let float64 = Id.gen ()
    let string = Id.gen ()
    let ty = Id.gen ()

    type dict_key = { fields : id StringMap.t }

    let dict_map = Hashtbl.create 0

    let dict (d : dict_key) =
      match Hashtbl.find_opt dict_map d with
      | Some id -> id
      | None ->
          let id = Id.gen () in
          Hashtbl.add dict_map d id;
          id
  end

  let rec _rec_block_start () =
    failwith "this is here to easier finding functions by 'and name' KEKW"

  and infer_var_type (var : inference_var) : value_type =
    let type_var = MyInference.get_type var in
    let t : value_type = InferVar (MyInference.new_var ()) in
    MyInference.set type_var (Type t : value);
    t

  and inference_unite_contexts (a : contexts_type) (b : contexts_type) :
      contexts_type =
    a
  (* TODO if a = b then a else raise @@ Inference.FailedUnite "contexts dont match" *)

  and inference_unite_types (a : value_type) (b : value_type) : value_type =
    match (a, b) with
    (* InferVars patterns must be at the top here *)
    | InferVar a, InferVar b ->
        MyInference.make_same a b;
        InferVar a
    | InferVar var, b ->
        MyInference.set var (Type b : value);
        (* TODO here something can be united? should result in that union? *)
        b
    | a, InferVar var ->
        MyInference.set var (Type a : value);
        (* TODO here something can be united? should result in that union? *)
        a
    | Binding a, Binding b when a.id = b.id -> Binding a
    | Binding _, _ -> failinfer ()
    | Void, Void -> Void
    | Void, _ -> failinfer ()
    | UnwindToken, UnwindToken -> UnwindToken
    | UnwindToken, _ -> failinfer ()
    | Never, Never -> Never
    | Never, _ -> failinfer ()
    | Ast, Ast -> Ast
    | Ast, _ -> failinfer ()
    | Bool, Bool -> Bool
    | Bool, _ -> failinfer ()
    | Int32, Int32 -> Int32
    | Int32, _ -> failinfer ()
    | Int64, Int64 -> Int64
    | Int64, _ -> failinfer ()
    | Float32, Float32 -> Float32
    | Float32, _ -> failinfer ()
    | Float64, Float64 -> Float64
    | Float64, _ -> failinfer ()
    | String, String -> String
    | String, _ -> failinfer ()
    | BuiltinMacro, _ -> failwith "inferred builtin macro type?"
    | Type, Type -> Type
    | Type, _ -> failinfer ()
    | Fn a, Fn b ->
        Fn
          {
            arg_type = inference_unite_types a.arg_type b.arg_type;
            result_type = inference_unite_types a.result_type b.result_type;
            contexts = inference_unite_contexts a.contexts b.contexts;
          }
    | Fn _, _ -> failinfer ()
    | Macro a, Macro b ->
        Macro
          {
            arg_type = inference_unite_types a.arg_type b.arg_type;
            result_type = inference_unite_types a.result_type b.result_type;
            contexts = inference_unite_contexts a.contexts b.contexts;
          }
    | Macro _, _ -> failinfer ()
    | Template a, Template b ->
        (if a != b then
           let compiled_a = ensure_compiled a in
           let compiled_b = ensure_compiled b in
           let bindings_a = pattern_bindings compiled_a.args in
           let bindings_b = pattern_bindings compiled_b.args in
           let vars_args : value =
             (* TODO pattern -> value with vars in place of bindings *)
             Dict
               {
                 fields =
                   StringMap.match_map
                     (fun _name a b : value ->
                       let var = MyInference.new_var () in
                       MyInference.make_same var a.value_type;
                       MyInference.make_same var b.value_type;
                       Var { id = Id.gen (); typ = InferVar var })
                     bindings_a bindings_b;
               }
           in
           let var_inst_a =
             call_compiled empty_contexts (ensure_compiled a) vars_args
           in
           let var_inst_b =
             call_compiled empty_contexts (ensure_compiled b) vars_args
           in
           ignore @@ inference_unite var_inst_a var_inst_b);
        Template a
    | Template _, _ -> failinfer ()
    | NewType _, _ -> failwith "todo newtype was inferred"
    | Dict a, Dict b ->
        Dict
          {
            fields =
              StringMap.match_map
                (fun _name -> inference_unite_types)
                a.fields b.fields;
          }
    | Dict _, _ -> failinfer ()
    | OneOf a, OneOf b ->
        OneOf
          (StringMap.match_map
             (fun _name a b ->
               match (a, b) with
               | None, None -> None
               | Some a, Some b -> Some (inference_unite_types a b)
               | None, Some _ | Some _, None -> failinfer ())
             a b)
    | OneOf _, _ -> failinfer ()
    | Union _, _ -> failwith "todo union"
    | Var a, Var b -> if a.id = b.id then Var a else failinfer ()
    | Var _, _ -> failinfer ()
    | MultiSet _, _ -> failwith "todo inferred multiset"

  and inference_unite (a : value) (b : value) : value =
    match (a, b) with
    | Binding a, Binding b when a.id = b.id -> Binding a
    | Binding _, _ -> failinfer ()
    | InferVar a, InferVar b ->
        MyInference.make_same a b;
        InferVar a
    | InferVar a, b ->
        MyInference.set a b;
        (* TODO here something can be united? should result in that union? *)
        b
    | a, InferVar b ->
        MyInference.set b a;
        (* TODO here something can be united? should result in that union? *)
        a
    | Var a, Var b when a.id = b.id -> Var a
    | Var _, _ -> failinfer ()
    | Void, Void -> Void
    | Void, _ -> failinfer ()
    | UnwindToken a, UnwindToken b ->
        if a = b then UnwindToken a else failinfer ()
    | UnwindToken _, _ -> failinfer ()
    | Ast a, Ast b -> if a = b then Ast a else failinfer ()
    | Ast _, _ -> failinfer ()
    | Macro _, _ -> failwith "inferred macro?"
    | Function _, _ -> failwith "inferred function?"
    | Template _, _ -> failwith "inferred template?"
    | BuiltinMacro _, _ -> failwith "inferred builtinmacro?"
    | BuiltinFn _, _ -> failwith "inferred builtinfn?"
    | Bool a, Bool b -> if a = b then Bool a else failinfer ()
    | Bool _, _ -> failinfer ()
    | Int32 a, Int32 b -> if a = b then Int32 a else failinfer ()
    | Int32 _, _ -> failinfer ()
    | Int64 a, Int64 b -> if a = b then Int64 a else failinfer ()
    | Int64 _, _ -> failinfer ()
    | Float64 a, Float64 b -> if a = b then Float64 a else failinfer ()
    | Float64 _, _ -> failinfer ()
    | String a, String b -> if a = b then String a else failinfer ()
    | String _, _ -> failinfer ()
    | Dict a, Dict b -> failwith "todo check inferred dicts"
    | Dict _, _ -> failinfer ()
    | Struct _, _ -> failwith "inferred struct?"
    | Variant _, _ -> failwith "inferred variant?"
    | Ref _, _ -> failwith "inferred ref?"
    | Type a, Type b -> Type (inference_unite_types a b)
    | Type _, _ -> failinfer ()

  and set_ir_type (ir : ir) (t : value_type) =
    MyInference.set (ir_data ir).type_var (Type t : value)

  and new_fn_type_vars () : fn_type_vars =
    {
      arg_type = MyInference.new_var ();
      result_type = MyInference.new_var ();
      contexts = MyInference.new_var ();
    }

  and fn_type_vars_to_type ({ arg_type; result_type; contexts } : fn_type_vars)
      : fn_type =
    {
      arg_type = InferVar arg_type;
      result_type = InferVar result_type;
      contexts = empty_contexts_type (* TODO InferVar contexts *);
    }

  and var_type (var : inference_var) : value_type =
    match MyInference.get_inferred var with
    | Some (Type t : value) -> t
    | Some value -> failwith @@ "ir inferred to be not a type but " ^ show value
    | None -> InferVar var

  and pattern_type (pattern : pattern) : value_type =
    var_type (pattern_data pattern).type_var

  and ir_type (ir : ir) : value_type = var_type (ir_data ir).type_var

  and set_pattern_type (pattern : pattern) (t : value_type) =
    MyInference.set (pattern_data pattern).type_var (Type t : value)

  and type_into_var t : MyInference.var =
    let var = MyInference.new_var () in
    MyInference.set var (Type t : value);
    var

  and contexts_type_into_var contexts =
    let var = MyInference.new_var () in
    (* TODO *)
    var

  and inferred_type var =
    match (MyInference.get_inferred var : value option) with
    | Some (Type t) -> t
    | Some _ -> failwith "inferred value expected to be a type"
    | None -> InferVar var

  and init_ir (ir : no_data ir_node) : ir =
    (* Log.trace @@ "initializing ir: " ^ show_ir_with_data (fun _ -> None) ir; *)
    try
      let known value =
        let type_var = MyInference.new_var () in
        MyInference.set type_var value;
        { type_var }
      in
      let known_type t = known (Type t : value) in
      let unknown () = { type_var = MyInference.new_var () } in
      let same_as other = { type_var = (ir_data other).type_var } in
      let result : ir =
        match ir with
        | Void { data = NoData } -> Void { data = known_type Void }
        | Const { value; data = NoData } ->
            Const
              { value; data = known_type @@ type_of_value ~ensure:false value }
        | Struct { body; data = NoData } -> Struct { body; data = same_as body }
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
            let result_type_var = MyInference.new_var () in
            List.iter
              (fun { pattern; body } ->
                MyInference.make_same result_type_var (ir_data body).type_var;
                MyInference.make_same value_var (pattern_data pattern).type_var)
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
        | OneOf { data = NoData; variants } ->
            OneOf { data = known_type Type; variants }
        | Let { data = NoData; pattern; value } ->
            MyInference.make_same (pattern_data pattern).type_var
              (ir_data value).type_var;
            Let { data = known_type Void; pattern; value }
        | Discard { data = NoData; value } ->
            Discard { data = known_type Void; value }
        | If { data = NoData; cond; then_case; else_case } ->
            set_ir_type cond Bool;
            MyInference.make_same (ir_data then_case).type_var
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
        | Unwinding { data = NoData; f } ->
            let f_type = new_fn_type_vars () in
            MyInference.set f_type.arg_type (Type UnwindToken : value);
            set_ir_type f @@ Fn (fn_type_vars_to_type f_type);
            Unwinding { data = { type_var = f_type.result_type }; f }
        | Call { data = NoData; f; args } -> (
            match (ir_data f).type_var |> MyInference.get_inferred with
            | Some (Type (Template t) : value) ->
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
                            value = InferVar (MyInference.new_var ());
                          }
                        |> init_ir;
                    }
                  |> init_ir
                in
                Call { data = NoData; f = instantiated; args } |> init_ir
            | _ ->
                let f_type = new_fn_type_vars () in
                MyInference.make_same f_type.arg_type (ir_data args).type_var;
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
            let field_type_var = MyInference.new_var () in
            MyInference.add_check (ir_data obj).type_var (fun value ->
                Log.trace @@ "checking field " ^ name ^ " of " ^ show value;
                match get_field_opt value name with
                | None -> failwith @@ "inferred type doesnt have field " ^ name
                | Some field ->
                    MyInference.set field_type_var field;
                    true);
            (* todo
               Inference.expand_dict obj.var name?
            *)
            (match default_value with
            | Some default ->
                MyInference.make_same field_type_var (ir_data default).type_var
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
        | Instantiate { data = NoData; captured; template; args } -> (
            match (eval_ir captured template).value with
            | Template t ->
                let tt = template_to_template_type t in
                let compiled = ensure_compiled tt in
                let sub =
                  pattern_bindings compiled.args
                  |> StringMap.map (fun _ : value ->
                         InferVar (MyInference.new_var ()))
                in
                let args_type : value =
                  InferVar (pattern_data compiled.args).type_var
                in
                Log.trace @@ show_pattern compiled.args ^ " initialized as "
                ^ show args_type;
                MyInference.set (ir_data args).type_var args_type;
                let result_type =
                  call_compiled empty_contexts compiled args_type
                in
                Log.trace @@ "subbing: " ^ show result_type;
                (* todo is subbing needed here? *)
                let result_type = result_type |> substitute_bindings sub in
                Log.trace @@ "sub result = " ^ show result_type;
                let args_value = (eval_ir t.captured args).value in
                (* todo is subbing needed here? *)
                ignore
                @@ inference_unite args_value
                     (pattern_to_value_with_binding_values compiled.args
                     |> substitute_bindings sub);
                Instantiate
                  {
                    data = known_type @@ value_to_type result_type;
                    captured = t.captured;
                    template;
                    args;
                  }
            | other -> failwith @@ show other ^ " is not a template")
      in
      Log.trace @@ "initialized ir: " ^ show_ir result;
      result
    with Failure _ as failure ->
      Log.error @@ "  while initializing ir " ^ ir_name ir;
      raise failure

  and pattern_to_value_with_binding_values (p : pattern) : value =
    match p with
    | Void { data = _ } -> Void
    | Placeholder { data } ->
        InferVar
          (let var = MyInference.new_var () in
           MyInference.make_same (MyInference.get_type var) data.type_var;
           var)
    | Binding { data = _; binding } -> Binding binding
    | Dict { fields; data = _ } ->
        Dict
          {
            fields =
              fields |> StringMap.map pattern_to_value_with_binding_values;
          }
    | Variant { data; name; value } ->
        Variant
          {
            typ = InferVar data.type_var;
            name;
            value = Option.map pattern_to_value_with_binding_values value;
          }
    | Union { data = _; a; b = _ } -> pattern_to_value_with_binding_values a

  and init_pattern (p : no_data pattern_node) : pattern =
    let known value =
      let type_var = MyInference.new_var () in
      MyInference.set type_var value;
      { type_var }
    in
    let known_type t = known (Type t : value) in
    let unknown () = { type_var = MyInference.new_var () } in
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
        MyInference.make_same (pattern_data a).type_var
          (pattern_data b).type_var;
        Union { data = same_as a; a; b }

  and show : value -> string = function
    | Binding { name; _ } -> name
    | Var { id; typ } -> "var " ^ Id.show id ^ " :: " ^ show_type typ
    | Ast ast -> "`(" ^ Ast.show ast ^ ")"
    | Variant { name; value; _ } ->
        name ^ show_or "" (fun value -> " " ^ show value) value
    | UnwindToken id -> "unwind token " ^ Id.show id
    | Void -> "void"
    | Macro f -> "macro " ^ show_fn f
    | BuiltinMacro _ -> "builtin_macro"
    | BuiltinFn { name; _ } -> "builtin_fn " ^ name
    | Template f -> "template " ^ show_fn f
    | Function f -> "function " ^ show_fn f
    | Int32 value -> Int32.to_string value
    | Int64 value -> Int64.to_string value
    | Float64 value -> Float.to_string value
    | Bool value -> Bool.to_string value
    | String value -> "\"" ^ String.escaped value ^ "\""
    | Dict { fields } ->
        "( "
        ^ StringMap.fold
            (fun name field acc ->
              (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show field)
            fields ""
        ^ " )"
    | Ref value -> "ref " ^ show !value
    | Struct _ -> "struct <...>"
    | Type t -> "type " ^ show_type t
    | InferVar var -> (
        match (MyInference.get_inferred var : value option) with
        | Some inferred -> show inferred
        | None -> "<not inferred " ^ MyInference.show_id var ^ ">")

  and show_fn_ast (f : fn_ast) : string =
    (match f.args with Some ast -> Ast.show ast | None -> "()")
    ^ (match f.returns with Some ast -> " -> " ^ Ast.show ast | None -> "")
    ^ (match f.contexts with Some ast -> " with " ^ Ast.show ast | None -> "")
    ^ " => " ^ Ast.show f.body

  and show_fn (f : fn) : string = if true then "<...>" else show_fn_ast f.ast

  and show_fn_type : fn_type -> string =
   fun { arg_type; contexts; result_type } ->
    show_type arg_type ^ " -> " ^ show_type result_type
    ^ Hashtbl.fold
        (fun value_type amount acc ->
          (if acc = "" then " with " else ", ")
          ^ Int.to_string amount ^ " of " ^ show_type value_type)
        contexts ""

  and show_contexts : contexts -> string =
   fun contexts ->
    Hashtbl.fold
      (fun typ values acc -> (if acc = "" then "" else ", ") ^ show_type typ)
      contexts ""

  and show_type : value_type -> string = function
    | Binding { name; _ } -> name
    | UnwindToken -> "unwind_token"
    | Never -> "!"
    | Ast -> "ast"
    | Void -> "void"
    | Bool -> "bool"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float32 -> "float32"
    | Float64 -> "float64"
    | String -> "string"
    | Fn f -> show_fn_type f
    | Macro f -> "macro " ^ show_fn_type f
    | Template f -> "template " ^ show_fn f
    | BuiltinMacro -> "builtin_macro"
    | Dict { fields } ->
        "{ "
        ^ StringMap.fold
            (fun name field_type acc ->
              (if acc = "" then "" else acc ^ ", ")
              ^ name ^ ": " ^ show_type field_type)
            fields ""
        ^ " }"
    | Type -> "type"
    | Union set ->
        Hashtbl.fold
          (fun t () acc -> (if acc = "" then "" else acc ^ " | ") ^ show_type t)
          set ""
    | OneOf variants ->
        StringMap.fold
          (fun name t acc ->
            (if acc = "" then "" else acc ^ " | ")
            ^ name
            ^ match t with Some t -> " of " ^ show_type t | None -> "")
          variants ""
    | NewType inner -> "newtype " ^ show_type inner
    | Var { id } -> "var " ^ Id.show id
    | InferVar var -> (
        match (MyInference.get_inferred var : value option) with
        | Some (Type inferred) -> show_type inferred
        | Some _ -> failwith "type was inferred as not a type wtf"
        | None -> "<not inferred " ^ MyInference.show_id var ^ ">")
    | MultiSet _ -> failwith "todo show multiset"

  and ir_name : 'a. 'a ir_node -> string = function
    | Void _ -> "void"
    | Struct _ -> "struct"
    | CreateImpl _ -> "create_impl"
    | GetImpl _ -> "get_impl"
    | CheckImpl _ -> "check_impl"
    | Match _ -> "match"
    | NewType _ -> "new_type"
    | Scope _ -> "scope"
    | OneOf _ -> "one_of"
    | TypeOf _ -> "type_of"
    | TypeOfValue _ -> "type_of_value"
    | Dict _ -> "dict"
    | Unwinding _ -> "unwinding"
    | WithContext _ -> "with_context"
    | CurrentContext _ -> "current_context"
    | Ast _ -> "ast"
    | Template _ -> "template"
    | Function _ -> "function"
    | FieldAccess _ -> "field_access"
    | Const _ -> "const"
    | Binding _ -> "binding"
    | Number _ -> "number"
    | String _ -> "string"
    | Discard _ -> "discard"
    | Then _ -> "then"
    | Call _ -> "call"
    | Instantiate _ -> "instantiate"
    | BuiltinFn _ -> "builtinfn"
    | If _ -> "if"
    | Let _ -> "let"

  and show_ir_with_data : ('a -> string option) -> 'a ir_node -> string =
   fun show_data ->
    let show_rec_pat : 'a pattern_node -> string =
     fun p -> show_pattern_with_data show_data p
    in
    let rec show_rec : 'a ir_node -> string =
     fun ir ->
      let ir_itself =
        match ir with
        | Void _ -> "void"
        | Struct { body; _ } -> "struct (" ^ show_rec body ^ ")"
        | CreateImpl { trait; value; impl; _ } ->
            "impl " ^ show_rec trait ^ " for " ^ show_rec value ^ " as "
            ^ show_rec impl
        | GetImpl { trait; value; _ } ->
            show_rec value ^ " as " ^ show_rec trait
        | CheckImpl { trait; value; _ } ->
            show_rec value ^ " impls " ^ show_rec trait
        | Match { value; branches; _ } ->
            let show_branch : string -> 'a match_branch -> string =
             fun acc branch ->
              acc ^ " | "
              ^ show_rec_pat branch.pattern
              ^ " => " ^ show_rec branch.body
            in
            "match " ^ show_rec value ^ " ("
            (* why can not we just have the for? *)
            ^ List.fold_left show_branch "" branches
            ^ ")"
        | OneOf { variants; _ } ->
            StringMap.fold
              (fun name variant acc ->
                (if acc = "" then "" else acc ^ " | ")
                ^ name
                ^
                match variant with
                | Some variant -> " of " ^ show_rec variant
                | None -> "")
              variants ""
        | NewType { def; _ } -> "newtype " ^ show_rec_pat def
        | Scope { expr; _ } -> "(" ^ show_rec expr ^ ")"
        | TypeOf { expr; _ } -> "typeof " ^ show_rec expr
        | TypeOfValue { expr; _ } -> "typeofvalue " ^ show_rec expr
        | Template { f; _ } -> "template " ^ show_fn f
        | Function _ -> "function"
        | Unwinding { f; _ } -> "unwinding " ^ show_rec f
        | WithContext { new_context; expr; _ } ->
            "with " ^ show_rec new_context ^ " (" ^ show_rec expr ^ ")"
        | CurrentContext { context_type; _ } ->
            "current_context " ^ show_type context_type
        | Dict { fields; _ } ->
            "{ "
            ^ StringMap.fold
                (fun name field acc ->
                  (if acc = "" then "" else acc ^ ", ")
                  ^ name ^ ": " ^ show_rec field)
                fields ""
            ^ " }"
        | Number { raw; _ } -> raw
        | Ast _ -> "ast"
        | Const { value; _ } -> "(const " ^ show value ^ ")"
        | FieldAccess { obj; name; _ } ->
            "(field " ^ show_rec obj ^ " " ^ name ^ ")"
        | BuiltinFn { f = { name; _ }; _ } -> "builtin_fn " ^ name
        | Discard { value; _ } -> "(discard " ^ show_rec value ^ ")"
        | Binding { binding; _ } -> "(binding " ^ binding.name ^ ")"
        | Call { f; args; _ } ->
            "(call " ^ show_rec f ^ " " ^ show_rec args ^ ")"
        | Instantiate { template; args; _ } ->
            "(instantiate " ^ show_rec template ^ " " ^ show_rec args ^ ")"
        | String { raw; _ } -> raw
        | Then { first; second; _ } ->
            "(then " ^ show_rec first ^ " " ^ show_rec second ^ ")"
        | If { cond; then_case; else_case; _ } ->
            "(if " ^ show_rec cond ^ " " ^ show_rec then_case ^ " "
            ^ show_rec else_case ^ ")"
        | Let { pattern; value; _ } ->
            "(let " ^ show_rec_pat pattern ^ " " ^ show_rec value ^ ")"
      in
      ir_itself ^ show_or "" (fun s -> s) (show_data (ir_data ir))
    in
    show_rec

  and show_ir : ir -> string =
   fun ir ->
    show_ir_with_data
      (fun data ->
        match MyInference.get_inferred data.type_var with
        | None -> None
        | Some inferred -> Some (" :: " ^ show inferred))
      ir

  and show_pattern : pattern -> string =
   fun p -> show_pattern_with_data (fun data -> None) p

  and show_pattern_with_data :
        'a. ('a -> string option) -> 'a pattern_node -> string =
   fun f pattern ->
    let rec show_rec : 'a. 'a pattern_node -> string = function
      | Placeholder _ -> "_"
      | Void _ -> "()"
      | Union { a; b; _ } -> show_rec a ^ " | " ^ show_rec b
      | Binding { binding; _ } -> binding.name
      | Variant { name; value; _ } ->
          name ^ show_or "" (fun value -> " " ^ show_rec value) value
      | Dict { fields; _ } ->
          "{ "
          ^ StringMap.fold
              (fun name field acc ->
                (if acc = "" then "" else acc ^ ", ")
                ^ name ^ ": " ^ show_rec field)
              fields ""
          ^ " }"
    in
    show_rec pattern

  and type_check_contexts (expected : contexts_type) (actual : contexts_type)
      (variables : type_var_map ref) : bool =
    Seq.for_all
      (fun ((expected_type, expected_amount) : value_type * int) ->
        let actual_amount =
          match Hashtbl.find_opt expected expected_type with
          | Some amount -> amount
          | None -> 0
        in
        actual_amount <= expected_amount)
      (Hashtbl.to_seq actual)

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
            (let tbl = Hashtbl.create 1 in
             Hashtbl.add tbl (InferVar f.vars.contexts) 1;
             tbl);
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
    | None ->
        let t =
          {
            id = Id.gen ();
            cached_template_type = None;
            vars =
              {
                arg_type = f.vars.arg_type;
                contexts = f.vars.contexts;
                result_type = MyInference.new_var ();
              };
            ast =
              {
                f.ast with
                body =
                  Complex
                    {
                      def =
                        {
                          name = "builtin_macro_typeof";
                          assoc = Left;
                          priority = 0.0;
                          parts = [];
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

  and type_of_value (value : value) ~(ensure : bool) : value_type =
    match value with
    | Binding { value_type; _ } -> InferVar value_type
    | InferVar var -> (
        match MyInference.get_inferred var with
        | Some value -> type_of_value value ~ensure
        | None -> infer_var_type var)
    | Var { typ; _ } -> typ
    | Ast _ -> Ast
    | UnwindToken _ -> UnwindToken
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
                (fun (value : value) -> type_of_value ~ensure value)
                data.locals;
          }
    | Type t -> Type

  and ir_data : 'data. 'data ir_node -> 'data = function
    | Void { data; _ }
    | Struct { data; _ }
    | CheckImpl { data; _ }
    | CreateImpl { data; _ }
    | GetImpl { data; _ }
    | Match { data; _ }
    | NewType { data; _ }
    | OneOf { data; _ }
    | Scope { data; _ }
    | Unwinding { data; _ }
    | WithContext { data; _ }
    | CurrentContext { data; _ }
    | TypeOf { data; _ }
    | TypeOfValue { data; _ }
    | Instantiate { data; _ }
    | Template { data; _ }
    | Function { data; _ }
    | Dict { data; _ }
    | Ast { data; _ }
    | FieldAccess { data; _ }
    | Const { data; _ }
    | Binding { data; _ }
    | Number { data; _ }
    | String { data; _ }
    | Discard { data; _ }
    | Then { data; _ }
    | Call { data; _ }
    | BuiltinFn { data; _ }
    | If { data; _ }
    | Let { data; _ } ->
        data

  and just_value value = { value; new_bindings = StringMap.empty }

  and update_locals =
    StringMap.union (fun _name _prev new_value -> Some new_value)

  (* this is lexeme val from_string : (Cparser.token, 'a) MenhirLib.Convert.traditional -> string -> 'a *)

  and pattern_match_opt (pattern : pattern) (value : value) :
      value StringMap.t option =
    try
      Log.trace
        ("trying to pattern match " ^ show value ^ " with "
       ^ show_pattern pattern);
      match pattern with
      | Placeholder _ -> Some StringMap.empty
      | Void _ -> ( match value with Void -> Some StringMap.empty | _ -> None)
      | Union { a; b; _ } -> (
          match pattern_match_opt a value with
          | Some result -> Some result
          | None -> pattern_match_opt b value)
      | Variant { name = pattern_name; value = value_pattern; _ } -> (
          match value with
          | Variant { name; value; _ } ->
              if name = pattern_name then
                match (value, value_pattern) with
                | None, None -> Some StringMap.empty
                | Some value, Some value_pattern ->
                    pattern_match_opt value_pattern value
                | Some _value, None ->
                    failwith "pattern did not expect a value but variant has"
                | None, Some _value_pattern ->
                    failwith "pattern expected a value but variant has none"
              else None
          | _ -> failwith "trying to match variant with a non-variant value")
      | Binding { binding = { name; _ }; _ } ->
          Some (StringMap.singleton name value)
      | Dict { fields = field_patterns; _ } -> (
          match value with
          | Dict { fields = field_values } ->
              let fields =
                StringMap.merge
                  (fun name pattern field_value ->
                    match (pattern, field_value) with
                    | Some pattern, Some value -> Some (pattern, value)
                    | Some _pattern, None ->
                        failwith (name ^ " is not a field in " ^ show value)
                    | None, Some _value ->
                        failwith ("pattern does not specify field " ^ name)
                    | None, None -> failwith "unreachable")
                  field_patterns field_values
              in
              StringMap.fold
                (fun _name (pattern, value) result ->
                  Option.bind result (fun result ->
                      Option.map
                        (fun field ->
                          StringMap.union
                            (fun name _old _new ->
                              failwith
                                (name
                               ^ " is specified multiple times in the pattern"))
                            result field)
                        (pattern_match_opt pattern value)))
                fields (Some StringMap.empty)
          | _ -> None)
    with Failure _ as failure ->
      Log.error @@ "  while pattern matching " ^ show value ^ " with "
      ^ show_pattern pattern;
      raise failure

  and pattern_match (pattern : pattern) (value : value) : value StringMap.t =
    match pattern_match_opt pattern value with
    | Some result -> result
    | None -> failwith "match failed"

  and get_local_opt (self : state) (name : string) : value option =
    match StringMap.find_opt name self.data.locals with
    | Some (Ref value) -> Some !value
    | Some local -> Some local
    | None ->
        let rec find_in_scopes (s : struct') =
          match StringMap.find_opt name s.data.locals with
          | Some local -> Some local
          | None -> (
              match s.parent with
              | Some parent -> find_in_scopes parent
              | None -> None)
        in
        find_in_scopes self.self

  and compile_ast_to_ir (self : state) (ast : ast) : compiled =
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
                  | Some (Binding binding) ->
                      Binding { binding; data = NoData } |> init_ir
                  | Some value -> Const { value; data = NoData } |> init_ir)
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
              failwith "wtf ast was compiled to a pattern but expected compiled"
          )
      | Syntax { def; value; _ } -> compile_ast_to_ir self value
    with Failure _ as failure ->
      Log.error @@ "  while compiling to ir: " ^ Ast.name ast;
      raise failure

  and compile_pattern (self : state) (pattern : ast option) : pattern =
    match pattern with
    | None -> Void { data = NoData } |> init_pattern
    | Some ast -> (
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
                        value_type = MyInference.new_var ();
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

  and pattern_data (pattern : pattern) : pattern_data =
    match pattern with
    | Placeholder { data; _ }
    | Variant { data; _ }
    | Union { data; _ }
    | Void { data; _ }
    | Binding { data; _ }
    | Dict { data; _ } ->
        data

  and substitute_fn_vars (vars : fn_type_vars) (state : state) : fn_type_vars =
    let sub var =
      InferVar var
      (* todo only locals? maybe should check in parents too? *)
      |> substitute_type_bindings state.data.locals
      |> (fun t : value -> Type t)
      |> MyInference.new_set_var
    in
    {
      arg_type = sub vars.arg_type;
      result_type = sub vars.result_type;
      contexts = sub vars.contexts;
    }

  and substitute_type_bindings (sub : value StringMap.t) (t : value_type) :
      value_type =
    match t with
    | Binding { name; _ } -> (
        match sub |> StringMap.find_opt name with
        | Some sub -> sub |> value_to_type
        | None -> t)
    | Var _ -> t
    | UnwindToken -> t
    | Never -> t
    | Ast -> t
    | Void -> t
    | Bool -> t
    | Int32 -> t
    | Int64 -> t
    | Float32 -> t
    | Float64 -> t
    | String -> t
    | Fn { arg_type; contexts; result_type } ->
        (* todo contexts *)
        Fn
          {
            arg_type = arg_type |> substitute_type_bindings sub;
            result_type = result_type |> substitute_type_bindings sub;
            contexts;
          }
    | Macro _ -> failwith @@ "todo Macro " ^ show_type t
    | Template _ -> failwith @@ "todo Template " ^ show_type t
    | BuiltinMacro -> failwith @@ "todo BuiltinMacro " ^ show_type t
    | Dict { fields } ->
        Dict { fields = fields |> StringMap.map (substitute_type_bindings sub) }
    | NewType _ -> failwith @@ "todo NewType " ^ show_type t
    | OneOf _ -> failwith @@ "todo OneOf " ^ show_type t
    | Union _ -> failwith @@ "todo Union " ^ show_type t
    | Type -> failwith @@ "todo Type " ^ show_type t
    | InferVar var -> (
        match MyInference.get_inferred var with
        | Some (Type t : value) -> t |> substitute_type_bindings sub
        | Some _ -> failwith "inferred as not type wtf"
        | None -> t)
    | MultiSet _ -> failwith @@ "todo MultiSet " ^ show_type t

  and substitute_bindings (sub : value StringMap.t) (value : value) : value =
    match value with
    | Binding { name; _ } -> (
        match sub |> StringMap.find_opt name with
        | Some sub -> sub
        | None -> value)
    | Var _ -> value
    | InferVar var -> (
        match MyInference.get_inferred var with
        | None -> value
        | Some inferred -> inferred |> substitute_bindings sub)
    | UnwindToken _ -> value
    | Ast _ -> value
    | Macro _ -> value
    | BuiltinMacro _ -> value
    | BuiltinFn _ -> value
    | Template _ -> value
    | Function _ -> value
    | Void -> value
    | Bool _ -> value
    | Int32 _ -> value
    | Int64 _ -> value
    | Float64 _ -> value
    | String _ -> value
    | Dict { fields } ->
        Dict { fields = fields |> StringMap.map (substitute_bindings sub) }
    | Struct _ -> value
    | Ref _ -> value
    | Type t -> Type (substitute_type_bindings sub t)
    | Variant { value; typ; name } ->
        Variant
          { typ; name; value = value |> Option.map (substitute_bindings sub) }

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
    match get_local_opt self name with
    | None -> failwith (name ^ " not found")
    | Some value -> (
        match value with
        | Function _ | BuiltinFn _ ->
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

  and eval_ast (self : state) (ast : ast) : evaled =
    let compiled = compile_ast_to_ir self ast in
    Log.trace ("compiled: " ^ show_ir compiled.ir);
    eval_ir self compiled.ir

  and log_state (level : Log.level) (self : state) : unit =
    let log = Log.with_level level in
    log "locals:";
    StringMap.iter
      (fun name value -> log ("  " ^ name ^ " = " ^ show value))
      self.data.locals

  and show_or : 'a. string -> ('a -> string) -> 'a option -> string =
   fun default f opt -> match opt with Some value -> f value | None -> default

  and type_id : value_type -> id = function
    | Binding { id; _ } -> id
    | UnwindToken -> TypeIds.unwind_token
    | Never -> TypeIds.never
    | Ast -> TypeIds.ast
    | Void -> TypeIds.void
    | Bool -> TypeIds.bool
    | Int32 -> TypeIds.int32
    | Int64 -> TypeIds.int64
    | Float32 -> TypeIds.float32
    | Float64 -> TypeIds.float64
    | String -> TypeIds.string
    | Fn f -> failwith "todo typeid fn"
    | Macro f -> failwith "todo typeid macro"
    | Template f -> failwith "todo typeid template"
    | BuiltinMacro -> failwith "todo typeid builtin_macro"
    | Dict { fields } ->
        TypeIds.dict { fields = fields |> StringMap.map type_id }
    | Type -> TypeIds.ty
    | Union _ -> failwith "todo typeid union"
    | OneOf variants -> failwith "todo typeid oneof"
    | NewType inner -> failwith "todo typeid newtype"
    | Var { id } -> id
    | InferVar var -> (
        match (MyInference.get_inferred var : value option) with
        | Some (Type inferred) -> type_id inferred
        | Some _ -> failwith "type was inferred as not a type wtf"
        | None -> failwith "can't get id for not inferred types")
    | MultiSet _ -> failwith "todo typeid multiset"

  and find_trait_id (trait : value) : id =
    match trait with
    | Template trait -> trait.id
    | Type trait -> type_id trait
    | _ -> failwith @@ "this value can not be a trait: " ^ show trait

  and check_impl (self : state) ({ trait; value; _ } : 'data get_impl) : bool =
    let trait = (eval_ir self trait).value in
    let trait_id = find_trait_id trait in
    let value = (eval_ir self value).value in
    let check (value : value) =
      match Hashtbl.find_opt trait_impls (type_id @@ value_to_type value) with
      | Some impls -> (
          match Hashtbl.find_opt impls trait_id with
          | Some _ -> true
          | None -> false)
      | None -> false
    in
    match value with
    | Type (InferVar var) ->
        MyInference.add_check var check;
        true
    | _ -> check value

  and get_impl (self : state) ({ trait; value; _ } : 'data get_impl) : value =
    let value = (eval_ir self value).value in
    let trait = (eval_ir self trait).value in
    match trait with
    | Type Type -> Type (value_to_type value)
    | _ -> (
        let ty = value_to_type value in
        let show_impls (ty : value_type) (tid : id)
            (impls : (id, value) Hashtbl.t) =
          Hashtbl.iter
            (fun trait impl ->
              Log.trace @@ "impl " ^ "trait (id=" ^ Id.show trait ^ ") for "
              ^ show_type ty ^ " as " ^ show impl)
            impls
        in
        let fail s =
          (* show_all_impls (); *)
          (* Log.trace s; *)
          failwith s
        in
        match Hashtbl.find_opt trait_impls (type_id ty) with
        | Some impls -> (
            match Hashtbl.find_opt impls (find_trait_id trait) with
            | Some impl -> impl
            | None ->
                Log.error @@ "get_impl failed: " ^ show trait
                ^ " is not implemented for " ^ show_type ty
                ^ " (see existing impls above)";
                failwith "get_impl")
        | None ->
            Log.error @@ "get_impl failed: " ^ show trait
            ^ " is not implemented for " ^ show_type ty ^ " (no impls found)";
            failwith "get_impl")

  and eval_ir (self : state) (ir : ir) : evaled =
    try
      log_state Log.Never self;
      let result_type = inferred_type (ir_data ir).type_var in
      Log.trace
        ("evaluating " ^ show_ir ir ^ " inferred as " ^ show_type @@ result_type);
      (* forward_expected_type ir expected_type; *)
      (* let result_type = (ir_data ir).result_type in *)
      let result =
        match ir with
        | Void _ -> just_value Void
        | Struct { body; _ } ->
            Log.trace "evaluating struct";
            let evaled = eval_ir self body in
            just_value
              (Struct
                 {
                   parent = Some self.self;
                   data =
                     { syntax = Syntax.empty; locals = evaled.new_bindings };
                 })
        | NewType { def; _ } ->
            just_value
              (Type (NewType (inferred_type (pattern_data def).type_var)))
        | Scope { expr; _ } -> just_value (eval_ir self expr).value
        | CreateImpl { trait; value; impl; _ } ->
            let ty = value_to_type (eval_ir self value).value in
            let trait = (eval_ir self trait).value in
            let trait_id : Id.t = find_trait_id trait in
            let impl = (eval_ir self impl).value in
            let type_id = type_id ty in
            if Hashtbl.find_opt trait_impls type_id |> Option.is_none then
              Hashtbl.add trait_impls type_id (Hashtbl.create 0);
            let type_impls = Hashtbl.find trait_impls type_id in
            Hashtbl.add type_impls trait_id impl;
            Log.trace @@ "added impl " ^ show trait ^ " for " ^ show_type ty
            ^ " as " ^ show impl;
            just_value Void
        | GetImpl e -> just_value (get_impl self e)
        | CheckImpl e -> just_value (Bool (check_impl self e))
        | Match { value; branches; _ } -> (
            let value = (eval_ir self value).value in
            let result : value option =
              List.find_map
                (fun (branch : ir_data match_branch) ->
                  pattern_match_opt branch.pattern value
                  |> Option.map (fun new_bindings ->
                         let self_with_new_bindings =
                           {
                             self with
                             data =
                               {
                                 self.data with
                                 locals =
                                   update_locals self.data.locals new_bindings;
                               };
                           }
                         in
                         (eval_ir self_with_new_bindings branch.body).value))
                branches
            in
            match result with
            | Some value -> just_value value
            | None -> failwith "match failed")
        | OneOf { variants; _ } ->
            just_value
              (Type
                 (OneOf
                    (StringMap.map
                       (Option.map (fun variant ->
                            value_to_type @@ (eval_ir self variant).value))
                       variants)))
        | TypeOf { expr; _ } ->
            just_value (Type (inferred_type (ir_data expr).type_var))
        | TypeOfValue { expr; _ } ->
            just_value
              (Type (type_of_value ~ensure:true (eval_ir self expr).value))
        | BuiltinFn { f; _ } -> just_value (BuiltinFn f)
        | Unwinding { f; _ } ->
            just_value
              (match (eval_ir self f).value with
              | Function f -> (
                  let compiled = ensure_compiled f in
                  let token = Id.gen () in
                  try call_compiled self.contexts compiled (UnwindToken token)
                  with Unwind (unwinded_token, value) ->
                    if unwinded_token = token then value
                    else raise (Unwind (unwinded_token, value)))
              | _ -> failwith "unwinding must take a function")
        | WithContext { new_context; expr; _ } ->
            let new_context = (eval_ir self new_context).value in
            let new_state =
              {
                self with
                contexts =
                  (let new_contexts = Hashtbl.copy self.contexts in
                   let context_type = type_of_value ~ensure:true new_context in
                   (* todo assert fully inferred *)
                   let new_list_of_this_context_type =
                     new_context
                     ::
                     (match Hashtbl.find_opt new_contexts context_type with
                     | Some list -> list
                     | None -> [])
                   in
                   Hashtbl.add new_contexts context_type
                     new_list_of_this_context_type;
                   new_contexts);
              }
            in
            just_value (eval_ir new_state expr).value
        | CurrentContext { context_type; _ } -> (
            let all_current =
              match Hashtbl.find_opt self.contexts context_type with
              | Some current -> current
              | None -> []
            in
            match head all_current with
            | Some top -> just_value top
            | None ->
                failwith
                  ("context not available: " ^ show_type context_type
                 ^ ", current contexts: "
                  ^ show_contexts self.contexts))
        | Dict { fields; _ } ->
            just_value
              (Dict
                 {
                   fields =
                     StringMap.map
                       (fun value -> (eval_ir self value).value)
                       fields;
                 })
        | Template { f; _ } ->
            just_value
              (Template
                 {
                   id = Id.gen ();
                   cached_template_type = None;
                   ast = f.ast;
                   vars = substitute_fn_vars f.vars self;
                   captured = self;
                   compiled = None;
                 })
        | Function { f; _ } ->
            just_value
              (Function
                 {
                   id = Id.gen ();
                   cached_template_type = None;
                   ast = f.ast;
                   vars = substitute_fn_vars f.vars self;
                   captured = self;
                   compiled = None;
                 })
        | Ast { def; ast_data; values; _ } ->
            just_value
              (Ast
                 (Complex
                    {
                      def;
                      data = ast_data;
                      values =
                        StringMap.map
                          (fun value ->
                            match (eval_ir self value).value with
                            | Ast ast -> ast
                            | _ -> failwith "expected an ast")
                          values;
                    }))
        | FieldAccess { obj; name; default_value; _ } ->
            let obj = (eval_ir self obj).value in
            let value =
              match get_field_opt obj name with
              | Some value -> value
              | None -> (
                  match default_value with
                  | Some default -> (eval_ir self default).value
                  | None ->
                      failwith
                        ("field " ^ name ^ " does not exist in " ^ show obj))
            in
            just_value value
        | Const { value; _ } -> just_value value
        | Binding { binding; _ } -> (
            match get_local_opt self binding.name with
            | None -> failwith (binding.name ^ " not found wtf, we are compiled")
            | Some value -> just_value value)
        | Number { raw = s; data } ->
            just_value
              (match inferred_type data.type_var with
              | Int32 -> Int32 (Int32.of_string s)
              | Int64 -> Int64 (Int64.of_string s)
              | Float64 -> Float64 (Float.of_string s)
              | t -> failwith (show_type t ^ " is not a number"))
        | String { value; _ } -> just_value (String value)
        | Discard { value = ir; _ } ->
            discard (eval_ir self ir).value;
            just_value Void
        | Then { first; second; _ } ->
            let first = eval_ir self first in
            discard first.value;
            let result =
              eval_ir
                {
                  self with
                  data =
                    {
                      self.data with
                      locals = update_locals self.data.locals first.new_bindings;
                    };
                }
                second
            in
            {
              result with
              new_bindings =
                StringMap.union
                  (fun _name _a b -> Some b)
                  first.new_bindings result.new_bindings;
            }
        | Instantiate { template; args; _ } ->
            let template = (eval_ir self template).value in
            Log.trace ("instantiating " ^ show template);
            let f =
              match template with
              | Template f ->
                  let compiled = ensure_compiled f in
                  call_compiled empty_contexts compiled
              | _ -> failwith @@ show template ^ " is not a template"
            in
            let args = (eval_ir self args).value in
            Log.trace @@ "instantiating with " ^ show args;
            (* todo memoization *)
            just_value (f args)
        | Call { f; args; _ } ->
            let f = (eval_ir self f).value in
            let (get_f_impl, vars) : (unit -> value -> value) * fn_type_vars =
              match f with
              | BuiltinFn f -> ((fun () -> f.impl), new_fn_type_vars ())
              | Function f | Macro f ->
                  ( (fun () -> call_compiled self.contexts @@ ensure_compiled f),
                    f.vars )
              | BuiltinMacro { impl; _ } ->
                  let f : value -> value = function
                    | Dict { fields } ->
                        let ir =
                          match
                            impl self
                              (StringMap.map
                                 (fun (value : value) : ast ->
                                   match value with
                                   | Ast ast -> ast
                                   | _ ->
                                       failwith
                                         "builtin macro arg must be dict of \
                                          asts")
                                 fields)
                              ~new_bindings:false
                          with
                          | Compiled { ir; _ } -> ir
                          | Pattern _ ->
                              failwith
                                "wtf builtin macro became pattern, expected \
                                 compiled"
                        in
                        (eval_ir self ir).value
                    | _ -> failwith "builtin macro arg must be dict of asts"
                  in
                  ( (fun () -> f),
                    (* todo actually infer, dont create new vars here *)
                    {
                      result_type = MyInference.new_var ();
                      contexts = MyInference.new_var ();
                      arg_type = MyInference.new_var ();
                    } )
              | _ -> failwith @@ show f ^ " - not a function"
            in
            let f_impl = get_f_impl () in
            let args = (eval_ir self args).value in
            Log.trace ("calling " ^ show f);
            Log.trace @@ "args = " ^ show args;
            let result = f_impl args in
            Log.trace @@ "result = " ^ show result;
            just_value result
        | If { cond; then_case; else_case; _ } ->
            let cond = eval_ir self cond in
            let self_with_new_bindings =
              {
                self with
                data =
                  {
                    self.data with
                    locals = update_locals self.data.locals cond.new_bindings;
                  };
              }
            in
            just_value
              (match cond.value with
              | Bool true -> eval_ir self_with_new_bindings then_case
              | Bool false -> eval_ir self_with_new_bindings else_case
              | _ -> failwith "condition must be a bool")
                .value
        | Let { pattern; value; _ } ->
            let value = (eval_ir self value).value in
            let result =
              { value = Void; new_bindings = pattern_match pattern value }
            in
            (let log = Log.trace in
             log "new bindings after let:";
             StringMap.iter
               (fun name value -> log @@ "  " ^ name ^ " = " ^ show value)
               result.new_bindings);
            result
      in
      let result =
        match result_type with
        | Type -> { result with value = Type (value_to_type result.value) }
        | _ -> result
      in
      Log.trace
        ("evaluated " ^ show_ir ir ^ " = " ^ show result.value ^ " as "
        ^ show_type (type_of_value ~ensure:false result.value)
        ^ "(expected " ^ show_type result_type ^ ")");
      result
    with Failure _ as failure ->
      Log.error @@ "  while evaluating " ^ ir_name ir;
      raise failure

  and value_to_type : value -> value_type = function
    | InferVar var -> (
        match MyInference.get_inferred var with
        | Some value -> value_to_type value
        | None ->
            let t : value_type = InferVar (MyInference.new_var ()) in
            MyInference.set var (Type t : value);
            t)
    | Void -> Void
    | Type t -> t
    | Dict { fields } -> Dict { fields = StringMap.map value_to_type fields }
    | Struct { data; _ } ->
        Dict { fields = StringMap.map value_to_type data.locals }
    | Template f -> Template f
    | Var { id; typ } ->
        ignore @@ inference_unite_types typ Type;
        Var { id }
    | Binding binding -> Binding binding
    | other -> failwith (show other ^ " is not a type")

  and ensure_compiled (f : fn) : compiled_fn =
    if Option.is_none f.compiled then (
      Log.trace ("compiling " ^ show_fn_ast f.ast);
      let compiled =
        try
          let args = compile_pattern f.captured f.ast.args in
          MyInference.make_same (pattern_data args).type_var f.vars.arg_type;
          let captured =
            {
              f.captured with
              data =
                {
                  f.captured.data with
                  locals =
                    update_locals f.captured.data.locals
                      (StringMap.map
                         (fun binding : value -> Binding binding)
                         (pattern_bindings args));
                };
            }
          in
          {
            captured = f.captured;
            where_clause =
              (match f.ast.where with
              | None -> Const { value = Bool true; data = NoData } |> init_ir
              | Some clause -> (compile_ast_to_ir captured clause).ir);
            args;
            result_type = InferVar f.vars.result_type;
            result_type_ir =
              (match f.ast.returns with
              | None -> None
              | Some ast -> Some (compile_ast_to_ir captured ast).ir);
            body = (compile_ast_to_ir captured f.ast.body).ir;
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

  and value_to_contexts_type (value : value) : contexts_type =
    let result = Hashtbl.create 1 in
    Hashtbl.add result (value_to_type value) 1;
    result

  and call_compiled (current_contexts : contexts) (f : compiled_fn)
      (args : value) : value =
    let captured =
      {
        f.captured with
        data =
          {
            f.captured.data with
            locals =
              update_locals f.captured.data.locals
                (StringMap.mapi
                   (fun name value ->
                     Log.trace ("called with arg " ^ name ^ " = " ^ show value);
                     value)
                   (pattern_match f.args args));
          };
      }
    in
    (* TODO reenable
       (match (eval_ir captured f.where_clause).value with
       | Bool true -> ()
       | Bool false ->
           Log.error @@ "where clause failed: " ^ show_ir f.where_clause
           ^ ", args = " ^ show args;
           failwith "where clause failed"
       | value ->
           failwith @@ "where clause evaluated to " ^ show value
           ^ ", expected a bool");
    *)
    Log.trace
      ("calling " ^ show_ir f.body ^ " with " ^ show args ^ " expecting "
     ^ show_type f.result_type);
    (eval_ir { captured with contexts = current_contexts } f.body).value

  and discard : value -> unit = function
    | Void -> ()
    | that ->
        failwith ("only void can be discarded (discarded " ^ show that ^ ")")

  and get_field_opt (obj : value) (field : string) : value option =
    match obj with
    | Dict { fields = dict } -> StringMap.find_opt field dict
    | Type (Dict { fields }) ->
        StringMap.find_opt field fields |> Option.map (fun t : value -> Type t)
    | Type (OneOf variants as typ) ->
        Option.map
          (fun (variant : value_type option) : value ->
            match variant with
            | Some variant ->
                (* TODO Type constructor type? *)
                BuiltinFn
                  {
                    name = "type constructor";
                    impl =
                      (fun value ->
                        Variant { name = field; typ; value = Some value });
                  }
            | None -> Variant { name = field; typ; value = None })
          (StringMap.find_opt field variants)
    | _ -> failwith @@ "can't get field " ^ field ^ " of " ^ show obj

  module Builtins = struct
    let type_ascribe : builtin_macro =
      {
        name = "type_ascribe";
        impl =
          (fun self args ~new_bindings ->
            let value =
              expand_ast self (StringMap.find "value" args) ~new_bindings
            in
            let typ =
              value_to_type (eval_ast self (StringMap.find "type" args)).value
            in
            let var =
              match value with
              | Compiled compiled -> (ir_data compiled.ir).type_var
              | Pattern pattern -> (pattern_data pattern).type_var
            in
            MyInference.set var (Type typ : value);
            value);
      }

    let call : builtin_macro =
      {
        name = "call";
        impl =
          (fun self args ~new_bindings ->
            let f = StringMap.find "f" args in
            let args = StringMap.find "args" args in
            match new_bindings with
            | true ->
                let name =
                  match f with
                  | Simple { token = Ident name; _ } -> name
                  | _ -> failwith "variant name must be simple ident"
                in
                let value = compile_pattern self (Some args) in
                Pattern
                  (Variant { name; value = Some value; data = NoData }
                  |> init_pattern)
            | false ->
                let f = compile_ast_to_ir self f in
                let args = compile_ast_to_ir self args in
                Compiled
                  {
                    ir =
                      Call { f = f.ir; args = args.ir; data = NoData }
                      |> init_ir;
                    new_bindings = StringMap.empty;
                  });
      }

    let instantiate_template : builtin_macro =
      {
        name = "instantiate_template";
        impl =
          (fun self args ~new_bindings ->
            let template =
              compile_ast_to_ir self (StringMap.find "template" args)
            in
            let args = compile_ast_to_ir self (StringMap.find "args" args) in
            Compiled
              {
                ir =
                  Instantiate
                    {
                      captured = self;
                      template = template.ir;
                      args = args.ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let then' : builtin_macro =
      {
        name = "then";
        impl =
          (fun self args ~new_bindings ->
            let a = compile_ast_to_ir self (StringMap.find "a" args) in
            let self_with_new_bindings =
              {
                self with
                data =
                  {
                    self.data with
                    locals = update_locals self.data.locals a.new_bindings;
                  };
              }
            in
            let b =
              match StringMap.find_opt "b" args with
              | Some b -> compile_ast_to_ir self_with_new_bindings b
              | None ->
                  {
                    ir = Void { data = NoData } |> init_ir;
                    new_bindings = StringMap.empty;
                  }
            in
            Compiled
              {
                ir =
                  Then { first = a.ir; second = b.ir; data = NoData } |> init_ir;
                new_bindings = update_locals a.new_bindings b.new_bindings;
              });
      }

    (*  todo *)
    let io_contexts = default_contexts_type

    let print : builtin_fn =
      {
        name = "print";
        impl =
          (function
          | String value ->
              print_endline value;
              Void
          | _ -> failwith "print expected a string");
      }

    let match' : builtin_macro =
      {
        name = "match";
        impl =
          (fun self args ~new_bindings ->
            let value = compile_ast_to_ir self (StringMap.find "value" args) in
            let rec collect_branches (ast : ast) : ir_data match_branch list =
              match ast with
              | Complex { def = { name = "combine_variants"; _ }; values; _ }
                -> (
                  let a = StringMap.find "a" values in
                  match StringMap.find_opt "b" values with
                  | Some b ->
                      List.append (collect_branches a) (collect_branches b)
                  | None -> collect_branches a)
              | Complex { def = { name = "function_def"; _ }; values; _ } ->
                  let args = StringMap.find "args" values in
                  let body = StringMap.find "body" values in
                  let pattern = compile_pattern self (Some args) in
                  let body =
                    compile_ast_to_ir
                      {
                        self with
                        data =
                          {
                            self.data with
                            locals =
                              update_locals self.data.locals
                                (pattern_bindings pattern
                                |> StringMap.map (fun binding : value ->
                                       Binding binding));
                          };
                      }
                      body
                  in
                  [ { pattern; body = body.ir } ]
              | _ -> failwith "match syntax wrong"
            in
            let branches = collect_branches (StringMap.find "branches" args) in
            Compiled
              {
                ir =
                  Match { value = value.ir; branches; data = NoData } |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let if' : builtin_macro =
      {
        name = "if";
        impl =
          (fun self args ~new_bindings ->
            let cond = compile_ast_to_ir self (StringMap.find "cond" args) in
            let self_with_new_bindings =
              {
                self with
                data =
                  {
                    self.data with
                    locals = update_locals self.data.locals cond.new_bindings;
                  };
              }
            in
            let then' =
              (compile_ast_to_ir self_with_new_bindings
                 (StringMap.find "then" args))
                .ir
            in
            let else' =
              match StringMap.find_opt "else" args with
              | Some branch ->
                  (compile_ast_to_ir self_with_new_bindings branch).ir
              | None -> Void { data = NoData } |> init_ir
            in
            Compiled
              {
                ir =
                  If
                    {
                      cond = cond.ir;
                      then_case = then';
                      else_case = else';
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let dict_fn f : value -> value = function
      | Dict { fields = args } -> f args
      | _ -> failwith "expected dict (dict_fn)"

    let int32_binary_op_with name lhs rhs f : builtin_fn =
      {
        name = "binary " ^ name;
        impl =
          dict_fn (fun args ->
              let lhs = StringMap.find lhs args in
              let rhs = StringMap.find rhs args in
              match (lhs, rhs) with
              | Int32 lhs, Int32 rhs -> Int32 (f lhs rhs)
              | _ -> failwith "only floats");
      }

    let int32_binary_op name = int32_binary_op_with name "lhs" "rhs"

    let float64_fn name f : builtin_fn =
      {
        name;
        impl =
          (function
          | Float64 value -> Float64 (f value) | _ -> failwith "only floats");
      }

    let int32_fn name f : builtin_fn =
      {
        name;
        impl =
          (function
          | Int32 value -> Int32 (f value) | _ -> failwith "only floats");
      }

    let single_arg_fn fn_name arg_name f : builtin_fn =
      {
        name = fn_name;
        impl =
          dict_fn (fun args ->
              let value = StringMap.find arg_name args in
              f value);
      }

    let float64_macro fn_name arg_name f =
      let f = float64_fn fn_name f in
      single_arg_fn fn_name arg_name f.impl

    let int32_macro fn_name arg_name f =
      let f = int32_fn fn_name f in
      single_arg_fn fn_name arg_name f.impl

    let int32_unary_op name = int32_macro ("unary " ^ name) "x"

    let scope : builtin_macro =
      {
        name = "scope";
        impl =
          (fun self args ~new_bindings ->
            let e = StringMap.find "e" args in
            match new_bindings with
            | true -> Pattern (compile_pattern self (Some e))
            | false ->
                Compiled
                  {
                    ir =
                      Scope
                        { expr = (compile_ast_to_ir self e).ir; data = NoData }
                      |> init_ir;
                    new_bindings = StringMap.empty;
                  });
      }

    let with_context : builtin_macro =
      {
        name = "with_context";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let new_context =
              compile_ast_to_ir self (StringMap.find "new_context" args)
            in
            let expr = compile_ast_to_ir self (StringMap.find "expr" args) in
            Compiled
              {
                ir =
                  WithContext
                    {
                      expr = expr.ir;
                      new_context = new_context.ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let comptime : builtin_macro =
      {
        name = "comptime";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let value = StringMap.find "value" args in
            Compiled
              {
                ir =
                  Const { value = (eval_ast self value).value; data = NoData }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let quote : builtin_macro =
      {
        name = "quote";
        impl =
          (fun self args ~new_bindings ->
            (* todo compile into pattern too *)
            let rec impl : ast -> ir = function
              | Complex
                  { def = { name = "builtin_macro_unquote"; _ }; values; _ } ->
                  let inner = StringMap.find "expr" values in
                  Log.trace ("unquoting" ^ Ast.show inner);
                  (compile_ast_to_ir self inner).ir
              | Nothing data ->
                  Const { value = Ast (Nothing data); data = NoData } |> init_ir
              | Simple token ->
                  Const { value = Ast (Simple token); data = NoData } |> init_ir
              | Complex { def; values; data = ast_data } ->
                  (Ast
                     {
                       def;
                       values = StringMap.map impl values;
                       ast_data;
                       data = NoData;
                     }
                    : no_data ir_node)
                  |> init_ir
              | Syntax { value; _ } -> impl value
            in
            let expr = StringMap.find "expr" args in
            Log.trace ("quoting " ^ Ast.show expr);
            (* todo *)
            Compiled { ir = impl expr; new_bindings = StringMap.empty });
      }

    let let' : builtin_macro =
      {
        name = "let";
        impl =
          (fun self args ~new_bindings ->
            let pattern = StringMap.find "pattern" args in
            let pattern = compile_pattern self (Some pattern) in
            let value = StringMap.find "value" args in
            let value = (compile_ast_to_ir self value).ir in
            Compiled
              {
                ir = Let { pattern; value; data = NoData } |> init_ir;
                new_bindings =
                  pattern_bindings pattern
                  |> StringMap.map (fun binding : value -> Binding binding);
              });
      }

    let const_let : builtin_macro =
      {
        name = "const_let";
        impl =
          (fun self args ~new_bindings ->
            let ir =
              match let'.impl self args ~new_bindings with
              | Compiled { ir; _ } -> ir
              | Pattern _ -> failwith "wtf const_let"
            in
            match ir with
            | Let { pattern; value; _ } ->
                let evaled = eval_ir self value in
                let let_ir =
                  Let
                    {
                      pattern;
                      value =
                        Const { value = evaled.value; data = NoData } |> init_ir;
                      data = NoData;
                    }
                  |> init_ir
                in
                let evaled : evaled = eval_ir self let_ir in
                Compiled { ir = let_ir; new_bindings = evaled.new_bindings }
            | _ -> failwith "let compiled into not a let wtf?");
      }

    let template_def : builtin_macro =
      {
        name = "template_def";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let fn_ast : fn_ast =
              {
                args = Some (StringMap.find "args" args);
                returns = StringMap.find_opt "returns" args;
                where = StringMap.find_opt "where" args;
                body = StringMap.find "body" args;
                contexts = None;
              }
            in
            Compiled
              {
                ir =
                  Template
                    {
                      f =
                        {
                          captured = self;
                          ast = fn_ast;
                          cached_template_type = None;
                          id = Id.gen ();
                          vars = new_fn_type_vars ();
                          compiled = None;
                        };
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let function_def : builtin_macro =
      {
        name = "function_def";
        impl =
          (fun self args ~new_bindings ->
            let fn_ast : fn_ast =
              {
                args = StringMap.find_opt "args" args;
                returns = StringMap.find_opt "returns" args;
                where = StringMap.find_opt "where" args;
                body = StringMap.find "body" args;
                contexts = StringMap.find_opt "contexts" args;
              }
            in
            Compiled
              {
                ir =
                  Function
                    {
                      f =
                        {
                          ast = fn_ast;
                          id = Id.gen ();
                          cached_template_type = None;
                          vars = new_fn_type_vars ();
                          captured = self;
                          compiled = None;
                        };
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let field_access : builtin_macro =
      {
        name = "field_access";
        impl =
          (fun self args ~new_bindings ->
            (* todo compile to pattern *)
            let obj = StringMap.find "obj" args in
            let obj = (compile_ast_to_ir self obj).ir in
            let field = StringMap.find "field" args in
            let default_value = StringMap.find_opt "default_value" args in
            let default_value =
              Option.map
                (fun ast ->
                  Log.trace ("default = " ^ Ast.show ast);
                  (compile_ast_to_ir self ast).ir)
                default_value
            in
            match field with
            | Simple { token = Ident name; _ } ->
                Compiled
                  {
                    ir =
                      FieldAccess { obj; name; default_value; data = NoData }
                      |> init_ir;
                    new_bindings = StringMap.empty;
                  }
            | _ -> failwith "field access must be using an ident");
      }

    let field : builtin_macro =
      {
        name = "field";
        impl =
          (fun self args ~new_bindings ->
            let name = StringMap.find "name" args in
            let value = StringMap.find "value" args in
            let name =
              match name with
              | Ast.Simple { token = Ident name; _ } -> name
              | _ -> failwith "field name must be an ident"
            in
            match new_bindings with
            | true ->
                Pattern
                  (Dict
                     {
                       fields =
                         StringMap.singleton name
                           (compile_pattern self (Some value));
                       data = NoData;
                     }
                  |> init_pattern)
            | false ->
                Compiled
                  {
                    ir =
                      (Dict
                         {
                           fields =
                             StringMap.singleton name
                               (compile_ast_to_ir self value : compiled).ir;
                           data = NoData;
                         }
                        : _ ir_node)
                      |> init_ir;
                    new_bindings = StringMap.empty;
                  });
      }

    let current_context : builtin_macro =
      {
        name = "current_context";
        impl =
          (fun self args ~new_bindings ->
            let context_type = StringMap.find "context_type" args in
            Compiled
              {
                ir =
                  CurrentContext
                    {
                      context_type =
                        value_to_type (eval_ast self context_type).value;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let typeof : builtin_macro =
      {
        name = "typeof";
        impl =
          (fun self args ~new_bindings ->
            (* todo compile to pattern? maybe? *)
            let expr = StringMap.find "expr" args in
            Compiled
              {
                ir =
                  TypeOf
                    {
                      captured = self;
                      expr = (compile_ast_to_ir self expr).ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    (*  todo fn instead of macro *)
    let typeofvalue : builtin_macro =
      {
        name = "typeofvalue";
        impl =
          (fun self args ~new_bindings ->
            (* todo pattern? *)
            let expr = StringMap.find "expr" args in
            Compiled
              {
                ir =
                  TypeOfValue
                    {
                      captured = self;
                      expr = (compile_ast_to_ir self expr).ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let tuple : builtin_macro =
      {
        name = "tuple";
        impl =
          (fun self args ~new_bindings ->
            let a = StringMap.find "a" args in
            let a = expand_ast self a ~new_bindings in
            let pattern_fields : expanded_macro -> _ = function
              | Pattern (Dict { fields; _ }) -> fields
              | Pattern _ -> failwith "expected a dict (pattern fields)"
              | Compiled _ -> failwith "wtf tuple pattern fields"
            in
            let fields : expanded_macro -> _ = function
              | Compiled { ir = Dict { fields; _ }; _ } -> fields
              | Compiled _ -> failwith "expected a dict (fields)"
              | Pattern _ -> failwith "wtf tuple fields"
            in
            match StringMap.find_opt "b" args with
            | Some b -> (
                let b = expand_ast self b ~new_bindings in
                match new_bindings with
                | true ->
                    Pattern
                      (Dict
                         {
                           fields =
                             StringMap.union
                               (fun name _a _b ->
                                 failwith (name ^ " is specified multiple times"))
                               (pattern_fields a) (pattern_fields b);
                           data = NoData;
                         }
                      |> init_pattern)
                | false ->
                    Compiled
                      {
                        ir =
                          (Dict
                             {
                               fields =
                                 StringMap.union
                                   (fun name _a _b ->
                                     failwith
                                       (name ^ " is specified multiple times"))
                                   (fields a) (fields b);
                               data = NoData;
                             }
                            : _ ir_node)
                          |> init_ir;
                        new_bindings = StringMap.empty;
                      })
            | None -> (
                match new_bindings with
                | true ->
                    Pattern
                      (Dict { fields = pattern_fields a; data = NoData }
                      |> init_pattern)
                | false ->
                    Compiled
                      {
                        ir =
                          (Dict { fields = fields a; data = NoData }
                            : _ ir_node)
                          |> init_ir;
                        new_bindings = StringMap.empty;
                      }));
      }

    let macro : value -> value = function
      | Function f -> Macro f
      | other -> failwith ("expected a function, got " ^ show other)

    let cmp_fn name f : builtin_fn =
      {
        name;
        impl =
          dict_fn (fun args ->
              let lhs = StringMap.find "lhs" args in
              let rhs = StringMap.find "rhs" args in
              match (lhs, rhs) with
              | Int32 lhs, Int32 rhs -> Bool (f lhs rhs)
              | _ -> failwith "only int32 is supported for comparisons");
      }

    let dbg : builtin_fn =
      {
        name = "dbg";
        impl =
          (fun value ->
            print_endline
              (show value ^ " : "
              ^ show_type (type_of_value ~ensure:false value));
            Void);
      }

    let get_int32 : value -> int32 = function
      | Int32 value -> value
      | _ -> failwith "expected int32"

    let get_float64 : value -> float = function
      | Float64 value -> value
      | _ -> failwith "expected float64"

    let get_type : value -> value_type = function
      | Type value_type -> value_type
      | value -> failwith ("expected type, got " ^ show value)

    let function_type : builtin_fn =
      {
        name = "function_type";
        impl =
          (function
          | Dict { fields = args } ->
              let arg_type = value_to_type (StringMap.find "arg" args) in
              let result_type = value_to_type (StringMap.find "result" args) in
              let contexts =
                match StringMap.find_opt "contexts" args with
                | Some contexts -> value_to_contexts_type contexts
                | None -> default_contexts_type
              in
              let result : value =
                Type (Fn { arg_type; result_type; contexts })
              in
              Log.trace (show result);
              result
          | _ -> failwith "expected dict (function_type)");
      }

    let random_int32 : builtin_fn =
      {
        name = "random_int32";
        impl =
          (function
          | Dict { fields = args } ->
              let min = get_int32 (StringMap.find "min" args) in
              let max = get_int32 (StringMap.find "max" args) in
              Int32
                (Int32.add min
                   (Random.int32
                      (Int32.sub (Int32.add max (Int32.of_int 1)) min)))
          | _ -> failwith "expected dict (random_int32)");
      }

    let random_float64 : builtin_fn =
      {
        name = "random_float64";
        impl =
          (function
          | Dict { fields = args } ->
              let min = get_float64 (StringMap.find "min" args) in
              let max = get_float64 (StringMap.find "max" args) in
              Float64 (min +. Random.float (max -. min))
          | _ -> failwith "expected dict (random_float64)");
      }

    let panic : builtin_fn =
      {
        name = "panic";
        impl =
          (function
          | String s -> failwith s
          | _ -> failwith "panicked with not a string kekw");
      }

    let is_same_type : builtin_fn =
      {
        name = "is_same_type";
        impl =
          (function
          | Dict { fields } ->
              let a = get_type (StringMap.find "a" fields) in
              let b = get_type (StringMap.find "b" fields) in
              let result = type_id a = type_id b in
              Log.trace
                ("is_same_type " ^ show_type a ^ ", " ^ show_type b ^ " = "
               ^ Bool.to_string result);
              Bool result
          | _ -> failwith "expected dict (is_same_type)");
      }

    let unwinding : builtin_macro =
      {
        name = "unwinding";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let def = StringMap.find "def" args in
            Compiled
              {
                ir =
                  Unwinding
                    { f = (compile_ast_to_ir self def).ir; data = NoData }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let unwind : builtin_fn =
      {
        name = "unwind";
        impl =
          (function
          | Dict { fields } ->
              let token =
                match StringMap.find "token" fields with
                | UnwindToken token -> token
                | _ -> failwith "expected an unwind token"
              in
              let value = StringMap.find "value" fields in
              raise (Unwind (token, value))
          | _ -> failwith "expected a dict in unwing args");
      }

    let union_variant : builtin_macro =
      {
        name = "union_variant";
        impl =
          (fun self args ~new_bindings ->
            let get_pattern = function
              | Pattern p -> p
              | Compiled _ -> failwith "expected a pattern"
            in
            let a =
              get_pattern
                (expand_ast self (StringMap.find "a" args) ~new_bindings)
            in
            let b =
              get_pattern
                (expand_ast self (StringMap.find "b" args) ~new_bindings)
            in
            match new_bindings with
            | true -> Pattern (Union { a; b; data = NoData } |> init_pattern)
            | false -> failwith "variant is only a pattern");
      }

    let single_variant : builtin_macro =
      {
        name = "single_variant";
        impl =
          (fun self args ~new_bindings ->
            let name =
              match StringMap.find "name" args with
              | Simple { token = Ident name; _ } -> name
              | _ -> failwith "name must be a simple ident"
            in
            if new_bindings then
              let value =
                Option.map
                  (fun value -> compile_pattern self (Some value))
                  (StringMap.find_opt "type" args)
              in
              Pattern (Variant { name; value; data = NoData } |> init_pattern)
            else
              let typ =
                Option.map
                  (fun typ -> (compile_ast_to_ir self typ).ir)
                  (StringMap.find_opt "type" args)
              in
              Compiled
                {
                  ir =
                    OneOf
                      { variants = StringMap.singleton name typ; data = NoData }
                    |> init_ir;
                  new_bindings = StringMap.empty;
                });
      }

    let combine_variants : builtin_macro =
      {
        name = "combine_variants";
        impl =
          (fun self args ~new_bindings ->
            match new_bindings with
            | false ->
                let get name : ir option StringMap.t =
                  let ir =
                    (compile_ast_to_ir self (StringMap.find name args)).ir
                  in
                  match ir with
                  | OneOf { variants; _ } -> variants
                  | _ -> failwith "expected oneof"
                in
                let a = get "a" in
                let b = get "b" in
                let variants =
                  StringMap.union
                    (fun name _a _b ->
                      failwith (name ^ " is specified multiple times"))
                    a b
                in
                Compiled
                  {
                    ir = OneOf { variants; data = NoData } |> init_ir;
                    new_bindings = StringMap.empty;
                  }
            | true -> failwith "combine variants is only a expr");
      }

    let create_impl : builtin_macro =
      {
        name = "create_impl";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let value = StringMap.find "value" args in
            let trait = StringMap.find "trait" args in
            let impl = StringMap.find "impl" args in
            Compiled
              {
                ir =
                  CreateImpl
                    {
                      captured = self;
                      value = (compile_ast_to_ir self value).ir;
                      trait = (compile_ast_to_ir self trait).ir;
                      impl = (compile_ast_to_ir self impl).ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let get_impl : builtin_macro =
      {
        name = "get_impl";
        impl =
          (fun self args ~new_bindings ->
            let value = StringMap.find "value" args in
            let trait = StringMap.find "trait" args in
            Compiled
              {
                ir =
                  GetImpl
                    {
                      captured = self;
                      value = (compile_ast_to_ir self value).ir;
                      trait = (compile_ast_to_ir self trait).ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let check_impl : builtin_macro =
      {
        name = "check_impl";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let value = StringMap.find "value" args in
            let trait = StringMap.find "trait" args in
            Compiled
              {
                ir =
                  CheckImpl
                    {
                      captured = self;
                      value = (compile_ast_to_ir self value).ir;
                      trait = (compile_ast_to_ir self trait).ir;
                      data = NoData;
                    }
                  |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let make_void : builtin_macro =
      {
        name = "make_void";
        impl =
          (fun _self _args ~new_bindings ->
            match new_bindings with
            | true -> Pattern (Void { data = NoData } |> init_pattern)
            | false ->
                Compiled
                  {
                    ir = Void { data = NoData } |> init_ir;
                    new_bindings = StringMap.empty;
                  });
      }

    let struct_def : builtin_macro =
      {
        name = "struct_def";
        impl =
          (fun self args ~new_bindings ->
            assert (not new_bindings);
            let body = StringMap.find "body" args in
            let body = (compile_ast_to_ir self body).ir in
            Compiled
              {
                ir = Struct { body; data = NoData } |> init_ir;
                new_bindings = StringMap.empty;
              });
      }

    let new_typevar : builtin_fn =
      {
        name = "new_typevar";
        impl =
          (fun _void ->
            Log.trace "making new type var";
            Type (Var { id = Id.gen () }));
      }

    let placeholder_fn : builtin_fn =
      {
        name = "placeholder";
        impl = (fun _ -> InferVar (MyInference.new_var ()));
      }

    let placeholder_macro : builtin_macro =
      {
        name = "placeholder";
        impl =
          (fun self args ~new_bindings ->
            match new_bindings with
            | true -> Pattern (Placeholder { data = NoData } |> init_pattern)
            | false ->
                Compiled
                  {
                    ir =
                      Const
                        {
                          value = InferVar (MyInference.new_var ());
                          data = NoData;
                        }
                      |> init_ir;
                    new_bindings = StringMap.empty;
                  });
      }

    let builtin_macros : builtin_macro list =
      [
        make_void;
        struct_def;
        create_impl;
        get_impl;
        check_impl;
        single_variant;
        combine_variants;
        unwinding;
        current_context;
        typeof;
        typeofvalue;
        type_ascribe;
        call;
        then';
        if';
        match';
        quote;
        scope;
        let';
        const_let;
        function_def;
        field_access;
        field;
        tuple;
        template_def;
        instantiate_template;
        comptime;
        with_context;
        placeholder_macro;
      ]

    let builtin_fns : builtin_fn list =
      [
        placeholder_fn;
        function_type;
        random_int32;
        random_float64;
        new_typevar;
        unwind;
        print;
        dbg;
        int32_unary_op "+" (fun x -> x);
        int32_unary_op "-" Int32.neg;
        int32_binary_op "+" Int32.add;
        int32_binary_op "-" Int32.sub;
        int32_binary_op "*" Int32.mul;
        int32_binary_op "/" Int32.div;
        (* ( (binary_op ( Stdlib.rem ))); *)
        float64_fn "sin" sin;
        float64_fn "cos" cos;
        float64_fn "sqrt" sqrt;
        {
          name = "type_of_value";
          impl = (fun x -> Type (type_of_value ~ensure:true x));
        };
        single_arg_fn "macro"
          (* todo { _anyfield: ast } -> ast #  (Fn { arg_type = Ast; result_type = Ast }) *)
          "def" macro;
        cmp_fn "<" ( < );
        cmp_fn "<=" ( <= );
        cmp_fn "==" ( = );
        cmp_fn "!=" ( <> );
        cmp_fn ">" ( > );
        cmp_fn ">=" ( >= );
        is_same_type;
        panic;
        {
          name = "input";
          impl =
            (function
            | Void -> String (read_line ()) | _ -> failwith "expected void");
        };
        {
          name = "string_to_int32";
          impl =
            (function
            | String s -> Int32 (Int32.of_string s)
            | _ -> failwith "expected string");
        };
        {
          name = "string_to_float64";
          impl =
            (function
            | String s -> Float64 (Float.of_string s)
            | _ -> failwith "expected string");
        };
      ]

    let builtin_values : (string * value) List.t =
      [
        ("unwind_token", Type UnwindToken);
        ("void", Void);
        ("ast", Type Ast);
        ("bool", Type Bool);
        ("true", Bool true);
        ("false", Bool false);
        ("int32", Type Int32);
        ("int64", Type Int64);
        ("float32", Type Float32);
        ("float64", Type Float64);
        ("string", Type String);
        ("never", Type Never);
        ("type", Type Type);
      ]

    let all : value StringMap.t =
      StringMap.of_list
        ((builtin_macros
         |> List.map (fun (m : builtin_macro) ->
                ("builtin_macro_" ^ m.name, (BuiltinMacro m : value))))
        @ (builtin_fns
          |> List.map (fun (f : builtin_fn) ->
                 ("builtin_fn_" ^ f.name, (BuiltinFn f : value))))
        @ builtin_values)
  end

  let empty () : state =
    let self =
      {
        parent = None;
        data = { locals = StringMap.empty; syntax = Syntax.empty };
      }
    in
    let state =
      {
        self;
        data =
          {
            locals =
              StringMap.union
                (fun _key _prev value -> Some value)
                Builtins.all
                (StringMap.singleton "Self" (Struct self : value)
                  : value StringMap.t);
            syntax = Syntax.empty;
          };
        contexts = Hashtbl.create 0;
      }
    in
    state

  let eval (self : state ref) (s : string) ~(filename : string) : value =
    let filename = Span.Filename filename in
    !self.self.data <- !self.data;
    let tokens = Lexer.parse s filename in
    let ast = Ast.parse !self.data.syntax tokens filename in
    Log.trace (Ast.show ast);
    let ast = Ast.map (fun span -> { span }) ast in
    let result : evaled = eval_ast !self ast in
    let rec extend_syntax syntax = function
      | Ast.Syntax { def; value; _ } ->
          extend_syntax (Syntax.add_syntax def syntax) value
      | _ -> syntax
    in
    self :=
      {
        !self with
        data =
          {
            syntax = extend_syntax !self.data.syntax ast;
            locals = update_locals !self.data.locals result.new_bindings;
          };
      };
    result.value

  let eval_file (self : state ref) (filename : string) : value =
    let f = open_in filename in
    let contents = really_input_string f (in_channel_length f) in
    close_in f;
    let value = eval self contents ~filename in
    Log.trace ("after " ^ filename ^ " syntax:");
    Log.trace (Syntax.show !self.data.syntax);
    value
end

and Checker : Inference.Checker = struct
  type t = Impl.value

  let unite = Impl.inference_unite
  let show = Impl.show
end

and MyInference : (Inference.T with type inferred := Impl.value) =
  Inference.Make (Checker)
