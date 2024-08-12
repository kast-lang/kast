open Prelude
open Types

module Make
    (Interpreter : Modules.Interpreter)
    (Compiler : Modules.Compiler)
    (Inference : Modules.Inference)
    (Utils : Modules.Utils)
    (Show : Modules.Show)
    (TypeId : Modules.TypeId)
    (Javascript : Modules.Javascript) =
struct
  open Utils
  open Interpreter
  open Compiler
  open Show

  let type_ascribe : builtin_macro =
    {
      name = "type_ascribe";
      impl =
        (fun self args ~new_bindings ->
          let value =
            Compiler.expand_ast self (StringMap.find "value" args) ~new_bindings
          in
          let typ =
            value_to_type (eval_ast self (StringMap.find "type" args)).value
          in
          let var =
            match value with
            | Compiled compiled ->
                Log.trace @@ "ascribing ir " ^ show_ir compiled.ir ^ " with "
                ^ show_type typ;
                (ir_data compiled.ir).inference.type_var
            | Pattern pattern -> (pattern_data pattern).type_var
          in
          Log.trace @@ "ascribed " ^ Inference.show_id var ^ " with "
          ^ show_type typ;
          Inference.set var (Type typ : value);
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
                match compile_pattern self (Some f) with
                | Variant { name; value = None; data = _ } -> name
                | _ ->
                    failwith "call in pattern must be on variant with no data"
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
                    |> init_ir self;
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
                |> init_ir self;
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
                  ir = Void { data = NoData } |> init_ir self;
                  new_bindings = StringMap.empty;
                }
          in
          Compiled
            {
              ir =
                Then { first = a.ir; second = b.ir; data = NoData }
                |> init_ir self;
              new_bindings = update_locals a.new_bindings b.new_bindings;
            });
    }

  let print : builtin_fn =
    {
      name = "print";
      impl =
        (fun _fn_type s ->
          match s with
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
            | Complex
                {
                  def = { name = "builtin macro merge_multiset"; _ };
                  values;
                  _;
                } -> (
                let a = StringMap.find "a" values in
                match StringMap.find_opt "b" values with
                | Some b ->
                    List.append (collect_branches a) (collect_branches b)
                | None -> collect_branches a)
            | Complex
                { def = { name = "builtin macro function_def"; _ }; values; _ }
              ->
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
                              (Compiler.pattern_bindings pattern
                              |> StringMap.map (fun binding : local ->
                                     { value = Binding binding; binding }));
                        };
                    }
                    body
                in
                [ { pattern; body = body.ir } ]
            | _ -> failwith @@ "match syntax wrong: " ^ Ast.show ast
          in
          let branches = collect_branches (StringMap.find "branches" args) in
          Compiled
            {
              ir =
                Match { value = value.ir; branches; data = NoData }
                |> init_ir self;
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
            | None -> Void { data = NoData } |> init_ir self
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
                |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let dict_fn f : fn_type -> value -> value =
   fun _fn_type args ->
    match args with
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
        (fun _fn_type args ->
          match args with
          | Float64 value -> Float64 (f value)
          | _ -> failwith "only floats");
    }

  let int32_fn name f : builtin_fn =
    {
      name;
      impl =
        (fun _fn_type -> function
          | Int32 value -> Int32 (f value)
          | _ -> failwith "only floats");
    }

  let single_arg_fn fn_name arg_name f : builtin_fn =
    {
      name = fn_name;
      impl =
        (fun fn_type ->
          dict_fn
            (fun args ->
              let value = StringMap.find arg_name args in
              f fn_type value)
            fn_type);
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
                    |> init_ir self;
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
                |> init_ir self;
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
                |> init_ir self;
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
            | Complex { def = { name = "builtin macro unquote"; _ }; values; _ }
              ->
                let inner = StringMap.find "expr" values in
                Log.trace ("unquoting" ^ Ast.show inner);
                (compile_ast_to_ir self inner).ir
            | Nothing data ->
                Const { value = Ast (Nothing data); data = NoData }
                |> init_ir self
            | Simple token ->
                Const { value = Ast (Simple token); data = NoData }
                |> init_ir self
            | Complex { def; values; data = ast_data } ->
                (Ast
                   {
                     def;
                     values = StringMap.map impl values;
                     ast_data;
                     data = NoData;
                   }
                  : no_data ir_node)
                |> init_ir self
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
              ir = Let { pattern; value; data = NoData } |> init_ir self;
              new_bindings =
                pattern_bindings pattern
                |> StringMap.map (fun binding : local ->
                       { value = Binding binding; binding });
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
                      Const { value = evaled.value; data = NoData }
                      |> init_ir self;
                    data = NoData;
                  }
                |> init_ir self
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
                        shared_compiled_args = ref None;
                        ast = fn_ast;
                        cached_template_type = None;
                        id = Id.gen ();
                        vars = Compiler.new_fn_type_vars ();
                        compiled = None;
                      };
                    data = NoData;
                  }
                |> init_ir self;
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
                        shared_compiled_args = ref None;
                        id = Id.gen ();
                        cached_template_type = None;
                        vars = new_fn_type_vars ();
                        captured = self;
                        compiled = None;
                      };
                    data = NoData;
                  }
                |> init_ir self;
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
                    |> init_ir self;
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
                    |> init_ir self;
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
                |> init_ir self;
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
                |> init_ir self;
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
                |> init_ir self;
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
                        |> init_ir self;
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
                        (Dict { fields = fields a; data = NoData } : _ ir_node)
                        |> init_ir self;
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
        (fun _fn_type value ->
          print_endline
            (show value ^ " :: " ^ show_type (type_of_value ~ensure:true value));
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
        (fun _fn_type -> function
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
        (fun _fn_type -> function
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
        (fun _fn_type -> function
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
        (fun _fn_type -> function
          | String s -> failwith s
          | _ -> failwith "panicked with not a string kekw");
    }

  let is_same_type : builtin_fn =
    {
      name = "is_same_type";
      impl =
        (fun _fn_type -> function
          | Dict { fields } ->
              let a = get_type (StringMap.find "a" fields) in
              let b = get_type (StringMap.find "b" fields) in
              let result = TypeId.get a = TypeId.get b in
              Log.trace
                ("is_same_type " ^ show_type a ^ ", " ^ show_type b ^ " = "
               ^ Bool.to_string result);
              Bool result
          | _ -> failwith "expected dict (is_same_type)");
    }

  let unwindable_block : builtin_macro =
    {
      name = "unwindable_block";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let def = StringMap.find "def" args in
          Compiled
            {
              ir =
                UnwindableBlock
                  { f = (compile_ast_to_ir self def).ir; data = NoData }
                |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let unwind : builtin_fn =
    {
      name = "unwind";
      impl =
        (fun _fn_type -> function
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

  type _ Effect.t += DelimitedYield : (id * value) -> value Effect.t

  let delimited_block : builtin_fn =
    {
      name = "delimited_block";
      impl =
        (fun fn_type -> function
          | Dict { fields } ->
              let handler = StringMap.find "handler" fields in
              let body = StringMap.find "body" fields in
              let this_token_id = Id.gen () in
              let body () =
                eval_call body
                  (DelimitedToken this_token_id : value)
                  empty_contexts
              in
              Effect.Deep.try_with body ()
                {
                  effc =
                    (fun (type a) (eff : a Effect.t) ->
                      match eff with
                      | DelimitedYield (token_id, value)
                        when token_id = this_token_id ->
                          Some
                            (fun (k : (a, _) Effect.Deep.continuation) ->
                              let handler_args : value =
                                Dict
                                  {
                                    fields =
                                      StringMap.of_list
                                        [
                                          ("value", value);
                                          ( "resume",
                                            BuiltinFn
                                              {
                                                f =
                                                  {
                                                    name =
                                                      "resume "
                                                      ^ Id.show this_token_id;
                                                    impl =
                                                      (fun _fn_type resume_arg ->
                                                        Effect.Deep.continue k
                                                          resume_arg);
                                                  };
                                                (* TODO actually set correctly *)
                                                ty = None;
                                              } );
                                        ];
                                  }
                              in
                              eval_call handler handler_args empty_contexts)
                      | _ -> None);
                }
          | _ -> failwith "expected a dict with delimited block args");
    }

  let delimited_yield : builtin_fn =
    {
      name = "delimited_yield";
      impl =
        (fun _fn_type -> function
          | Dict { fields } ->
              let token_id =
                match StringMap.find "token" fields with
                | DelimitedToken token -> token
                | _ -> failwith "expected an delimit token"
              in
              let value = StringMap.find "value" fields in
              Effect.perform (DelimitedYield (token_id, value))
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

  let variant : builtin_macro =
    {
      name = "variant";
      impl =
        (fun self args ~new_bindings ->
          let variant_name =
            match StringMap.find "variant" args with
            | Simple { token = Ident name; _ } -> name
            | _ -> failwith "variant name must be simple ident"
          in
          match new_bindings with
          | true ->
              Pattern
                (Variant { name = variant_name; value = None; data = NoData }
                |> init_pattern)
          | false ->
              Compiled
                {
                  ir =
                    ConstructVariant
                      {
                        ty =
                          StringMap.find_opt "type" args
                          |> Option.map (fun type_ast ->
                                 value_to_type (eval_ast self type_ast).value);
                        variant = variant_name;
                        value =
                          StringMap.find_opt "value" args
                          |> Option.map (fun ast ->
                                 (compile_ast_to_ir self ast).ir);
                        data = NoData;
                      }
                    |> init_ir self;
                  new_bindings = StringMap.empty;
                });
    }

  let single_variant : builtin_macro =
    {
      name = "single_variant";
      impl =
        (fun self args ~new_bindings ->
          let name =
            match StringMap.find "name" args with
            | Simple { token = Ident name; _ } -> name
            | other ->
                failwith @@ "name must be a simple ident, got " ^ Ast.show other
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
                  |> init_ir self;
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
                match StringMap.find_opt name args with
                | Some ast -> (
                    let ir = (compile_ast_to_ir self ast).ir in
                    match ir with
                    | OneOf { variants; _ } -> variants
                    | _ -> failwith @@ "expected oneof, got " ^ show_ir ir)
                | None -> StringMap.empty
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
                  ir = OneOf { variants; data = NoData } |> init_ir self;
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
                |> init_ir self;
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
                |> init_ir self;
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
                |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let make_void : builtin_macro =
    {
      name = "make_void";
      impl =
        (fun self _args ~new_bindings ->
          match new_bindings with
          | true -> Pattern (Void { data = NoData } |> init_pattern)
          | false ->
              Compiled
                {
                  ir = Void { data = NoData } |> init_ir self;
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
          let struct_data : state_data =
            { locals = StringMap.empty; syntax = Syntax.empty }
          in
          let struct_state : state =
            {
              builtins = self.builtins;
              contexts = self.contexts;
              data = struct_data;
              self = { parent = Some self.self; data = struct_data };
            }
          in
          let body = compile_ast_to_ir struct_state body in
          Compiled
            {
              ir =
                Struct
                  {
                    body = body.ir;
                    field_types =
                      body.new_bindings
                      |> StringMap.map (fun local ->
                             InferVar local.binding.value_type);
                    data = NoData;
                  }
                |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let new_typevar : builtin_fn =
    {
      name = "new_typevar";
      impl =
        (fun _fn_type _void ->
          Log.trace "making new type var";
          Type (Var { id = Id.gen () }));
    }

  let placeholder_fn : builtin_fn =
    {
      name = "placeholder";
      impl = (fun _ _ -> InferVar (Inference.new_var ()));
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
                      { value = InferVar (Inference.new_var ()); data = NoData }
                    |> init_ir self;
                  new_bindings = StringMap.empty;
                });
    }

  let rec make_pattern_mutable (type a) (pattern : a pattern_node) :
      a pattern_node =
    match pattern with
    | Void data -> Void data
    | Binding { binding; data } ->
        Binding { binding = { binding with mut = true }; data }
    | Placeholder data -> Placeholder data
    | Dict { fields; data } ->
        Dict { fields = fields |> StringMap.map make_pattern_mutable; data }
    | Variant { name; value; data } ->
        Variant { name; value = value |> Option.map make_pattern_mutable; data }
    | Union { a; b; data } ->
        Union { a = make_pattern_mutable a; b = make_pattern_mutable b; data }

  let mutable_pattern : builtin_macro =
    {
      name = "mutable_pattern";
      impl =
        (fun self args ~new_bindings ->
          assert new_bindings;
          let pattern = StringMap.find "pattern" args in
          let pattern = compile_pattern self (Some pattern) in
          Pattern (make_pattern_mutable pattern));
    }

  let rec pattern_with_existing_bindings (self : state) (pattern : pattern) :
      pattern =
    match pattern with
    | Void data -> Void data
    | Binding { binding; data } ->
        let existing =
          match get_local_value_opt self binding.name with
          | Some (Binding existing) -> existing
          | Some _ -> (* TODO FIX this is a hack *) binding
          | None -> failwith @@ binding.name ^ " not found in current scope"
        in
        Binding { binding = existing; data }
    | Placeholder data -> Placeholder data
    | Dict { fields; data } ->
        Dict
          {
            fields =
              fields |> StringMap.map (pattern_with_existing_bindings self);
            data;
          }
    | Variant { name; value; data } ->
        Variant
          {
            name;
            value = value |> Option.map (pattern_with_existing_bindings self);
            data;
          }
    | Union { a; b; data } ->
        Union
          {
            a = pattern_with_existing_bindings self a;
            b = pattern_with_existing_bindings self b;
            data;
          }

  let assign : builtin_macro =
    {
      name = "assign";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let pattern =
            compile_pattern self (Some (StringMap.find "pattern" args))
          in
          let pattern = pattern_with_existing_bindings self pattern in
          let value =
            (compile_ast_to_ir self @@ StringMap.find "value" args).ir
          in
          Compiled
            {
              ir = Assign { pattern; value; data = NoData } |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let builtin : builtin_macro =
    {
      name = "builtin";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let name =
            match (eval_ast self (StringMap.find "name" args)).value with
            | String s -> s
            | value ->
                failwith @@ "builtin name supposed to be a string: "
                ^ show value
          in
          Compiled
            {
              ir = Builtin { name; data = NoData } |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let import_cache : value StringMap.t ref = ref StringMap.empty

  let import : builtin_macro =
    {
      name = "import";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let args_ast = StringMap.find "path" args in
          let path = (eval_ast self args_ast).value in
          let path =
            match path with
            | String path -> path
            | _ -> failwith @@ "import path not a string: " ^ show path
          in
          let path =
            if String.starts_with ~prefix:"/" path then path
            else
              let (Filename current_filename) = (Ast.data args_ast).span.file in
              Filename.concat (Filename.dirname current_filename) path
          in
          let value =
            match StringMap.find_opt path !import_cache with
            | Some cached -> cached
            | None ->
                let value =
                  eval_file (default_state () |> ref) ~filename:path
                in
                Log.trace @@ "evaluated for import: " ^ path;
                import_cache := StringMap.add path value !import_cache;
                value
          in
          Compiled
            {
              ir = Const { data = NoData; value } |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let use : builtin_macro =
    {
      name = "use";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let namespace =
            (eval_ast self (StringMap.find "namespace" args)).value
          in
          (* TODO work with not fully initialized namespaces *)
          let new_bindings = namespace_locals namespace in
          Compiled
            {
              ir = Use { namespace; data = NoData } |> init_ir self;
              new_bindings;
            });
    }

  let merge_multiset : builtin_macro =
    {
      name = "merge_multiset";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let get name : ir option =
            match StringMap.find_opt name args with
            | Some ast -> Some (compile_ast_to_ir self ast).ir
            | None -> None
          in
          let a = get "a" |> Option.get in
          let b = get "b" in
          Compiled
            {
              ir = MultiSet { a; b; data = NoData } |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let newtype : builtin_macro =
    {
      name = "newtype";
      impl =
        (fun self args ~new_bindings ->
          assert (not new_bindings);
          let def = StringMap.find "def" args in
          Compiled
            {
              ir =
                NewType { def = (compile_ast_to_ir self def).ir; data = NoData }
                |> init_ir self;
              new_bindings = StringMap.empty;
            });
    }

  let builtin_macros : builtin_macro list =
    [
      merge_multiset;
      newtype;
      use;
      import;
      builtin;
      assign;
      variant;
      mutable_pattern;
      make_void;
      struct_def;
      create_impl;
      get_impl;
      check_impl;
      single_variant;
      combine_variants;
      unwindable_block;
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

  let eq : builtin_fn =
    {
      name = "==";
      impl =
        dict_fn (fun args ->
            let a = StringMap.find "lhs" args in
            let b = StringMap.find "rhs" args in
            Bool
              (match (a, b) with
              | Void, Void -> true
              | Bool a, Bool b -> a = b
              | Int32 a, Int32 b -> a = b
              | Int64 a, Int64 b -> a = b
              | Float64 a, Float64 b -> a = b
              | String a, String b -> a = b
              | _ ->
                  failwith @@ "== doesnt work for " ^ show a ^ " and " ^ show b));
    }

  let compile_to_js : builtin_fn =
    {
      name = "compile_to_js";
      impl =
        (fun _fn_type value ->
          let js = Javascript.compile_value value in
          Dict
            {
              fields =
                StringMap.of_list
                  [
                    ("code", (String js.code : value));
                    ("var", String (Javascript.var_name js.var));
                  ];
            });
    }

  let builtin_fns : builtin_fn list =
    [
      compile_to_js;
      delimited_block;
      delimited_yield;
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
        impl = (fun _ x -> Type (type_of_value ~ensure:true x));
      };
      single_arg_fn "macro"
        (* todo { _anyfield: ast } -> ast #  (Fn { arg_type = Ast; result_type = Ast }) *)
        "def" (fun _ -> macro);
      cmp_fn "<" ( < );
      cmp_fn "<=" ( <= );
      eq;
      cmp_fn "!=" ( <> );
      cmp_fn ">" ( > );
      cmp_fn ">=" ( >= );
      is_same_type;
      panic;
      {
        name = "input";
        impl =
          (fun _fn_type -> function
            | Void -> String (read_line ())
            | _ -> failwith "expected void");
      };
      {
        name = "string_to_int32";
        impl =
          (fun _fn_type -> function
            | String s -> Int32 (Int32.of_string s)
            | _ -> failwith "expected string");
      };
      {
        name = "string_to_float64";
        impl =
          (fun _fn_type -> function
            | String s -> Float64 (Float.of_string s)
            | _ -> failwith "expected string");
      };
    ]

  let builtin_values : (string * value) List.t =
    [
      ("unwind_token", Type UnwindToken);
      ("delimited_token", Type DelimitedToken);
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

  let all () : value StringMap.t =
    StringMap.of_list
      ((builtin_macros
       |> List.map (fun (m : builtin_macro) ->
              ("macro " ^ m.name, (BuiltinMacro m : value))))
      @ (builtin_fns
        |> List.map (fun (f : builtin_fn) ->
               ("fn " ^ f.name, (BuiltinFn { f; ty = None } : value))))
      @ builtin_values)
end
