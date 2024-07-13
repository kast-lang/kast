open Prelude

type ast_data = { span : Span.span; mutable ir : Ir.t option }
type ast = ast_data Ast.node

type value =
  | Ast of ast
  | Macro of fn
  | BuiltinMacro of builtin_macro
  | BuiltinFn of builtin_fn
  | Function of fn
  | Void
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Float64 of float
  | String of string
  | Dict of value StringMap.t
  | Struct of struct'
  | Ref of value ref
  | Type of value_type

and value_type =
  | Any
  | Ast
  | Void
  | Bool
  | Int32
  | Int64
  | Float32
  | Float64
  | String
  | Fn of { arg_type : value_type; result_type : value_type }
  | Macro
  | Dict of { fields : value_type StringMap.t }
  | Type

and builtin_fn = {
  impl : value -> value;
  arg_type : value_type;
  result_type : value_type;
}

and builtin_macro = state -> ast StringMap.t -> new_bindings:bool -> compiled

and 'data ir_node =
  | Void of { data : 'data }
  | Dict of { fields : 'data ir_node StringMap.t; data : 'data }
  | Ast of {
      def : Syntax.syntax_def;
      ast_data : ast_data;
      values : 'data ir_node StringMap.t;
      data : 'data;
    }
  | FieldAccess of { obj : 'data ir_node; name : string; data : 'data }
  | Const of { value : value; data : 'data }
  | Binding of { binding : binding; data : 'data }
  | Number of { raw : string; data : 'data }
  | String of { raw : string; value : string; data : 'data }
  | Discard of { value : 'data ir_node; data : 'data }
  | Then of { first : 'data ir_node; second : 'data ir_node; data : 'data }
  | Call of { f : 'data ir_node; args : 'data ir_node; data : 'data }
  | BuiltinFn of { f : builtin_fn; data : 'data }
  | If of {
      cond : 'data ir_node;
      then_case : 'data ir_node;
      else_case : 'data ir_node;
      data : 'data;
    }
  | Let of { pattern : pattern; value : 'data ir_node; data : 'data }

and ir_data = { mutable result_type : value_type option }
and ir = ir_data ir_node

and fn = {
  captured : state;
  args_pattern : ast option;
  body : ast;
  mutable compiled : compiled_fn option;
}

and compiled_fn = { captured : state; args : pattern; body : ir }
and pattern = Void | Binding of binding
and evaled = { value : value; new_bindings : value StringMap.t }
and compiled = { ir : ir; new_bindings : state_local StringMap.t }
and struct' = { parent : struct' option; mutable data : state_data }
and state = { self : struct'; data : state_data }
and state_data = { locals : state_local StringMap.t; syntax : Syntax.syntax }
and state_local = Value of value | Binding of binding
and binding = { name : string; mutable value_type : value_type option }

let rec show = function
  | Ast ast -> "`(" ^ Ast.show ast ^ ")"
  | Void -> "void"
  | Macro _ -> "macro <...>"
  | BuiltinMacro _ -> "builtin_macro"
  | BuiltinFn _ -> "builtin"
  | Function f ->
      "function "
      ^ (match f.args_pattern with Some ast -> Ast.show ast | None -> "()")
      ^ " => " ^ Ast.show f.body
  | Int32 value -> Int32.to_string value
  | Int64 value -> Int64.to_string value
  | Float64 value -> Float.to_string value
  | Bool value -> Bool.to_string value
  | String value -> "\"" ^ String.escaped value ^ "\""
  | Dict values ->
      "{ "
      ^ StringMap.fold
          (fun name value acc ->
            (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show value)
          values ""
      ^ " }"
  | Ref value -> show !value
  | Struct _ -> "struct <...>"
  | Type t -> "type " ^ show_type t

and show_type : value_type -> string = function
  | Any -> "any"
  | Ast -> "ast"
  | Void -> "void"
  | Bool -> "bool"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | String -> "string"
  | Fn { arg_type; result_type } ->
      show_type arg_type ^ " -> " ^ show_type result_type
  | Macro -> "macro"
  | Dict { fields } ->
      "{ "
      ^ StringMap.fold
          (fun name field_type acc ->
            (if acc = "" then "" else acc ^ ", ")
            ^ name ^ ": " ^ show_type field_type)
          fields ""
      ^ " }"
  | Type -> "type"

and same_types (a : value_type) (b : value_type) =
  match (a, b) with
  | Any, Any -> true
  | Ast, Ast -> true
  | Void, Void -> true
  | Bool, Bool -> true
  | Int32, Int32 -> true
  | Int64, Int64 -> true
  | Float32, Float32 -> true
  | Float64, Float64 -> true
  | String, String -> true
  | ( Fn { arg_type = arg_a; result_type = result_a },
      Fn { arg_type = arg_b; result_type = result_b } ) ->
      same_types arg_a arg_b && same_types result_a result_b
  | Macro, Macro -> true (* todo *)
  | Dict { fields = a }, Dict { fields = b } -> StringMap.equal same_types a b
  | Type, Type -> true
  | _, _ -> false

let rec type_of_value : value -> value_type = function
  | Ast _ -> Ast
  | Void -> Void
  | Macro _ -> Macro
  | BuiltinMacro _ -> Macro
  | BuiltinFn { arg_type; result_type; _ } -> Fn { arg_type; result_type }
  | Function f ->
      Log.info "getting type of fun";
      Fn
        {
          arg_type = Any;
          result_type = infer_types (ensure_compiled f).body None;
        }
  | Int32 _ -> Int32
  | Int64 _ -> Int64
  | Float64 _ -> Float64
  | Bool _ -> Bool
  | String _ -> String
  | Dict values -> Dict { fields = StringMap.map type_of_value values }
  | Ref _ -> failwith "todo"
  | Struct _ -> failwith "todo"
  | Type _ -> Type

and ir_data : 'data. 'data ir_node -> 'data = function
  | Void { data; _ }
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

and init_ir_data () : ir_data = { result_type = None }

and just_value value expected_type =
  (match expected_type with
  | Some Any -> ()
  | Some expected_type ->
      let actual_type = type_of_value value in
      if not (same_types actual_type expected_type) then
        failwith
          ("value is of wrong type (expected " ^ show_type expected_type
         ^ ", got " ^ show_type actual_type ^ ")")
  | None -> ());
  { value; new_bindings = StringMap.empty }

and update_locals =
  StringMap.union (fun _name _prev new_value -> Some new_value)

and pattern_match_opt (pattern : pattern) (value : value) :
    value StringMap.t option =
  match pattern with
  | Void -> ( match value with Void -> Some StringMap.empty | _ -> None)
  | Binding { name; _ } -> Some (StringMap.singleton name value)

and pattern_match (pattern : pattern) (value : value) : value StringMap.t =
  match pattern_match_opt pattern value with
  | Some result -> result
  | None -> failwith "match failed"

and get_local_opt (self : state) (name : string) : state_local option =
  match StringMap.find_opt name self.data.locals with
  | Some local -> Some local
  | None ->
      let rec find_in_scopes s =
        match StringMap.find_opt name s.data.locals with
        | Some local -> Some local
        | None -> (
            match s.parent with
            | Some parent -> find_in_scopes parent
            | None -> None)
      in
      find_in_scopes self.self

and get_local_value_opt (self : state) (name : string) : value option =
  Option.map
    (function
      | Value (Ref value) -> !value
      | Value other -> other
      | Binding _ -> failwith (name ^ " is a runtime value"))
    (get_local_opt self name)

and compile_ast (self : state) (ast : ast) ~(new_bindings : bool) : compiled =
  match ast with
  | Nothing _ ->
      { ir = Void { data = init_ir_data () }; new_bindings = StringMap.empty }
  | Simple { token; _ } ->
      {
        ir =
          (match token with
          | Ident ident -> (
              match get_local_opt self ident with
              | None ->
                  if new_bindings then
                    Binding
                      {
                        binding = { name = ident; value_type = None };
                        data = init_ir_data ();
                      }
                  else failwith (ident ^ " not found")
              | Some (Value value) -> Const { value; data = init_ir_data () }
              | Some (Binding binding) ->
                  Binding { binding; data = init_ir_data () })
          | Number raw -> Number { raw; data = init_ir_data () }
          | String { value; raw } ->
              String { value; raw; data = init_ir_data () }
          | Punctuation _ -> failwith "punctuation");
        new_bindings = StringMap.empty;
      }
  | Complex { def; values; _ } ->
      expand_macro self def.name values ~new_bindings
  | Syntax { def; value; _ } -> compile_ast self value ~new_bindings

and compile_pattern (self : state) (pattern : ast option) : pattern =
  match pattern with
  | None -> Void
  | Some ast -> (
      let ir = (compile_ast self ast ~new_bindings:true).ir in
      match ir with
      | Void _ -> Void
      | Dict _ -> failwith "todo pattern Dict"
      | Ast _ -> failwith "todo pattern Ast"
      | FieldAccess _ -> failwith "todo pattern FieldAccess"
      | Const _ -> failwith "todo pattern Const"
      | Binding { binding; _ } ->
          (* if ascribed *)
          binding.value_type <- (ir_data ir).result_type;
          Binding binding
      | Number _ -> failwith "todo pattern Number"
      | String _ -> failwith "todo pattern String"
      | Discard _ -> failwith "todo pattern Discard"
      | Then _ -> failwith "todo pattern Then"
      | Call _ -> failwith "todo pattern Call"
      | BuiltinFn _ -> failwith "todo pattern BuiltinFn"
      | If _ -> failwith "todo pattern If"
      | Let _ -> failwith "todo pattern Let")

and pattern_type (pattern : pattern) : value_type option =
  match pattern with Void -> Some Void | Binding binding -> binding.value_type

and pattern_bindings (pattern : pattern) : binding StringMap.t =
  match pattern with
  | Void -> StringMap.empty
  | Binding binding -> StringMap.singleton binding.name binding

and expand_macro (self : state) (name : string) (values : ast StringMap.t)
    ~(new_bindings : bool) : compiled =
  match get_local_opt self name with
  | None -> failwith (name ^ " not found")
  | Some (Binding _binding) ->
      failwith (name ^ " is a runtime value, not a macro")
  | Some (Value value) -> (
      match value with
      | BuiltinFn f ->
          let args : ir =
            Dict
              {
                fields =
                  StringMap.map
                    (fun arg -> (compile_ast self arg ~new_bindings).ir)
                    values;
                data = init_ir_data ();
              }
          in
          {
            ir =
              Call
                {
                  f = BuiltinFn { f; data = init_ir_data () };
                  args;
                  data = init_ir_data ();
                };
            new_bindings = StringMap.empty;
          }
      | BuiltinMacro f -> f self values ~new_bindings
      | Macro f -> (
          let values = Dict (StringMap.map (fun x -> Ast x) values) in
          Log.trace ("call macro with " ^ show values);
          match compile_and_call f values with
          | Ast new_ast ->
              Log.trace ("macro expanded to " ^ Ast.show new_ast);
              compile_ast self new_ast ~new_bindings
          | _ -> failwith "macro returned not an ast")
      | _ -> failwith (name ^ " is not a macro"))

and eval_ast (self : state) (ast : ast) (expected_type : value_type option) :
    evaled =
  let compiled = compile_ast self ast ~new_bindings:false in
  eval_ir self compiled.ir expected_type

and forward_expected_type (ir : ir) (expected_type : value_type option) : unit =
  let ir_data = ir_data ir in
  match expected_type with
  | Some expected_type -> (
      match ir_data.result_type with
      | Some actual_type ->
          if not (same_types actual_type expected_type) then
            failwith
              ("expected " ^ show_type expected_type ^ ", got "
             ^ show_type actual_type)
      | None -> ir_data.result_type <- Some expected_type)
  | None -> ()

and maybe_infer_types (ir : ir) (expected_type : value_type option) :
    value_type option =
  let ir_data = ir_data ir in
  forward_expected_type ir expected_type;
  match ir_data.result_type with
  | None ->
      (* infering type *)
      let actual_type : value_type option =
        match ir with
        | Void _ -> Some Void
        | BuiltinFn { f = { arg_type; result_type; _ }; _ } ->
            Some (Fn { arg_type; result_type })
        | Dict { fields; _ } ->
            let field_types =
              match ir_data.result_type with
              | Some (Dict { fields }) -> fields
              | _ -> StringMap.empty (* todo None? *)
            in
            Some
              (Dict
                 {
                   fields =
                     StringMap.mapi
                       (fun name value ->
                         infer_types value (StringMap.find_opt name field_types))
                       fields;
                 })
        | Ast _ -> Some Ast
        | FieldAccess { obj; name; _ } -> (
            match infer_types obj None with
            | Dict { fields } -> Some (StringMap.find name fields)
            | _ -> failwith "todo")
        | Const { value = Function _; _ } -> None (* maybe Some if compiled? *)
        | Const { value; _ } ->
            Log.info ("inferring type of " ^ show value);
            Some (type_of_value value)
        | Binding { binding; _ } -> binding.value_type
        | Number _ -> None
        | String _ -> Some String
        | Discard _ -> Some Void
        | Then { second; _ } -> maybe_infer_types second None
        | Call { f; _ } -> (
            match maybe_infer_types f None with
            | Some (Fn { result_type; _ }) -> Some result_type
            | Some _ -> failwith "not fun"
            | None -> None)
        | If { then_case; else_case; _ } -> (
            let then_case = maybe_infer_types then_case None in
            let else_case = maybe_infer_types else_case then_case in
            match (then_case, else_case) with
            | Some then_case, Some else_case ->
                if not (same_types then_case else_case) then
                  failwith
                    ("then and else diff types: " ^ show_type then_case
                   ^ " and " ^ show_type else_case);
                Some then_case
            | Some case, None | None, Some case -> Some case
            | None, None -> None)
        | Let _ -> Some Void
      in
      (match actual_type with
      | Some actual_type -> (
          match ir_data.result_type with
          | None -> ir_data.result_type <- Some actual_type
          | Some expected_type ->
              if not (same_types actual_type expected_type) then
                failwith
                  ("expected " ^ show_type expected_type ^ ", got "
                 ^ show_type actual_type))
      | None -> ());
      actual_type
  | Some result_type -> Some result_type

and infer_types (ir : ir) (expected_type : value_type option) : value_type =
  Option.get (maybe_infer_types ir expected_type)

and eval_ir (self : state) (ir : ir) (expected_type : value_type option) :
    evaled =
  let result_type = maybe_infer_types ir expected_type in
  (* forward_expected_type ir expected_type; *)
  (* let result_type = (ir_data ir).result_type in *)
  match ir with
  | Void _ -> just_value Void result_type
  | BuiltinFn { f; _ } -> just_value (BuiltinFn f) result_type
  | Dict { fields; _ } ->
      let field_types =
        match result_type with
        | Some (Dict { fields }) -> fields
        | Some _ -> failwith "not a dict"
        | None -> StringMap.empty (* todo None? *)
      in
      just_value
        (Dict
           (StringMap.mapi
              (fun name value ->
                (eval_ir self value (StringMap.find_opt name field_types)).value)
              fields))
        result_type
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
                      match (eval_ir self value (Some Ast)).value with
                      | Ast ast -> ast
                      | _ -> failwith "expected an ast")
                    values;
              }))
        result_type
  | FieldAccess { obj; name; _ } ->
      let obj = (eval_ir self obj None).value in
      just_value (get_field obj name) result_type
  | Const { value; _ } -> just_value value result_type
  | Binding { binding; _ } -> (
      match get_local_value_opt self binding.name with
      | None -> failwith (binding.name ^ " not found wtf, we are compiled")
      | Some value -> just_value value result_type)
  | Number { raw = s; data } ->
      just_value
        (match data.result_type with
        | None -> failwith "number type unspecified"
        | Some t -> (
            match t with
            | Int32 -> Int32 (Int32.of_string s)
            | Int64 -> Int64 (Int64.of_string s)
            | Float64 -> Float64 (Float.of_string s)
            | _ -> failwith (show_type t ^ " is not a number")))
        result_type
  | String { value; _ } -> just_value (String value) result_type
  | Discard { value = ir; _ } ->
      discard (eval_ir self ir None).value;
      just_value Void result_type
  | Then { first = a; second = b; _ } ->
      let a = eval_ir self a None in
      discard a.value;
      eval_ir
        {
          self with
          data =
            {
              self.data with
              locals =
                update_locals self.data.locals
                  (StringMap.map (fun value -> Value value) a.new_bindings);
            };
        }
        b result_type
  | Call { f; args; _ } ->
      let f = (eval_ir self f None).value in
      Log.trace ("calling " ^ show f);
      let f, args_type =
        match f with
        | BuiltinFn f -> (f.impl, Some f.arg_type)
        | Function f ->
            let compiled = ensure_compiled f in
            (call_compiled compiled, pattern_type compiled.args)
        | _ -> failwith "not a function"
      in
      Log.trace
        ("args_type = "
        ^
        match args_type with
        | Some args_type -> show_type args_type
        | None -> "<unknown>");
      let args = (eval_ir self args args_type).value in
      just_value (f args) result_type
  | If { cond; then_case; else_case; _ } -> (
      let common_type = result_type in
      Log.trace
        ("evaling if, type = "
        ^
        match common_type with
        | Some common_type -> show_type common_type
        | None -> "<unknown>");
      let cond = eval_ir self cond (Some Bool) in
      let self_with_new_bindings =
        {
          self with
          data =
            {
              self.data with
              locals =
                update_locals self.data.locals
                  (StringMap.map (fun value -> Value value) cond.new_bindings);
            };
        }
      in
      match cond.value with
      | Bool true -> eval_ir self_with_new_bindings then_case common_type
      | Bool false -> eval_ir self_with_new_bindings else_case common_type
      | _ -> failwith "condition must be a bool")
  | Let { pattern; value; _ } ->
      {
        value = Void;
        new_bindings = pattern_match pattern (eval_ir self value None).value;
      }

and compile_and_call (f : fn) (args : value) : value =
  call_compiled (ensure_compiled f) args

and ensure_compiled (f : fn) : compiled_fn =
  if Option.is_none f.compiled then (
    Log.trace ("compiling " ^ Ast.show f.body);
    f.compiled <-
      (let args = compile_pattern f.captured f.args_pattern in
       Some
         {
           captured = f.captured;
           args;
           body =
             (compile_ast
                {
                  f.captured with
                  data =
                    {
                      f.captured.data with
                      locals =
                        update_locals f.captured.data.locals
                          (StringMap.map
                             (fun binding : state_local -> Binding binding)
                             (pattern_bindings args));
                    };
                }
                f.body ~new_bindings:false)
               .ir;
         }));
  Option.get f.compiled

and call_compiled (f : compiled_fn) (args : value) : value =
  (eval_ir
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
                    Value value)
                  (pattern_match f.args args));
         };
     }
     f.body None)
    .value

and discard : value -> unit = function
  | Void -> ()
  | that -> failwith ("only void can be discarded (discarded " ^ show that ^ ")")

and get_field obj field =
  match obj with
  | Dict dict -> StringMap.find field dict
  | _ -> failwith "can't get field of this thang"

module Builtins = struct
  let type_ascribe : builtin_macro =
   fun self args ~new_bindings ->
    let value = compile_ast self (StringMap.find "value" args) ~new_bindings in
    let typ = (eval_ast self (StringMap.find "type" args) (Some Type)).value in
    match typ with
    | Type typ ->
        let ir_data = ir_data value.ir in
        if Option.is_some ir_data.result_type then
          failwith "type already specified";
        ir_data.result_type <- Some typ;
        value
    | _ -> failwith "type is not a type"

  let call : builtin_macro =
   fun self args ~new_bindings ->
    let f = compile_ast self (StringMap.find "f" args) ~new_bindings in
    let args = compile_ast self (StringMap.find "args" args) ~new_bindings in
    {
      ir = Call { f = f.ir; args = args.ir; data = init_ir_data () };
      new_bindings = StringMap.empty;
    }

  let then' : builtin_macro =
   fun self args ~new_bindings ->
    let a = compile_ast self (StringMap.find "a" args) ~new_bindings in
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
    match StringMap.find_opt "b" args with
    | Some b ->
        let b = compile_ast self_with_new_bindings b ~new_bindings in
        {
          ir = Then { first = a.ir; second = b.ir; data = init_ir_data () };
          new_bindings = update_locals a.new_bindings b.new_bindings;
        }
    | None ->
        {
          ir = Discard { value = a.ir; data = init_ir_data () };
          new_bindings = a.new_bindings;
        }

  let print : builtin_fn =
    {
      impl =
        (function
        | String value ->
            print_endline value;
            Void
        | _ -> failwith "print expected a string");
      arg_type = String;
      result_type = Void;
    }

  let if' : builtin_macro =
   fun self args ~new_bindings ->
    let cond = compile_ast self (StringMap.find "cond" args) ~new_bindings in
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
      (compile_ast self_with_new_bindings
         (StringMap.find "then" args)
         ~new_bindings)
        .ir
    in
    let else' =
      match StringMap.find_opt "else" args with
      | Some branch ->
          (compile_ast self_with_new_bindings branch ~new_bindings).ir
      | None -> Void { data = init_ir_data () }
    in
    {
      ir =
        If
          {
            cond = cond.ir;
            then_case = then';
            else_case = else';
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let dict_fn f = function Dict args -> f args | _ -> failwith "expected dict"

  let int32_binary_op_with lhs rhs f : builtin_fn =
    {
      impl =
        dict_fn (fun args ->
            let lhs = StringMap.find lhs args in
            let rhs = StringMap.find rhs args in
            match (lhs, rhs) with
            | Int32 lhs, Int32 rhs -> Int32 (f lhs rhs)
            | _ -> failwith "only floats");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list [ (lhs, (Int32 : value_type)); (rhs, Int32) ];
          };
      result_type = Int32;
    }

  let int32_binary_op = int32_binary_op_with "lhs" "rhs"

  let float64_fn f : builtin_fn =
    {
      impl =
        (function
        | Float64 value -> Float64 (f value) | _ -> failwith "only floats");
      arg_type = Float64;
      result_type = Float64;
    }

  let int32_fn f : builtin_fn =
    {
      impl =
        (function
        | Int32 value -> Int32 (f value) | _ -> failwith "only floats");
      arg_type = Int32;
      result_type = Int32;
    }

  let single_arg_fn arg_type name result_type f : builtin_fn =
    {
      impl =
        dict_fn (fun args ->
            let value = StringMap.find name args in
            f value);
      arg_type;
      result_type;
    }

  let float64_macro name f =
    let f = float64_fn f in
    single_arg_fn Float64 name Float64 f.impl

  let int32_macro name f =
    let f = int32_fn f in
    single_arg_fn Int32 name Int32 f.impl

  let int32_unary_op = int32_macro "x"

  let scope : builtin_macro =
   fun self args ~new_bindings ->
    let e = StringMap.find "e" args in
    {
      ir = (compile_ast self e ~new_bindings).ir;
      new_bindings = StringMap.empty;
    }

  let quote : builtin_macro =
   fun self args ~new_bindings ->
    let rec impl : ast -> ir = function
      | Complex { def = { name = "unquote"; _ }; values; _ } ->
          let inner = StringMap.find "expr" values in
          Log.trace ("unquoting" ^ Ast.show inner);
          (compile_ast self inner ~new_bindings).ir
      | Nothing data ->
          Const { value = Ast (Nothing data); data = init_ir_data () }
      | Simple token ->
          Const { value = Ast (Simple token); data = init_ir_data () }
      | Complex { def; values; data = ast_data } ->
          Ast
            {
              def;
              values = StringMap.map impl values;
              ast_data;
              data = init_ir_data ();
            }
      | Syntax { value; _ } -> impl value
    in
    let expr = StringMap.find "expr" args in
    Log.trace ("quoting " ^ Ast.show expr);
    { ir = impl expr; new_bindings = StringMap.empty }

  let let' : builtin_macro =
   fun self args ~new_bindings ->
    let pattern = StringMap.find "pattern" args in
    let pattern = compile_pattern self (Some pattern) in
    let value = StringMap.find "value" args in
    let value = (compile_ast self value ~new_bindings).ir in
    {
      ir = Let { pattern; value; data = init_ir_data () };
      new_bindings =
        StringMap.map
          (fun binding : state_local -> Binding binding)
          (pattern_bindings pattern);
    }

  let function_def : builtin_macro =
   fun self args ~new_bindings ->
    let args_pattern = StringMap.find_opt "args" args in
    let body = StringMap.find "body" args in
    {
      ir =
        Const
          {
            value =
              Function { captured = self; args_pattern; body; compiled = None };
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let field_access : builtin_macro =
   fun self args ~new_bindings ->
    let obj = StringMap.find "obj" args in
    let obj = (compile_ast self obj ~new_bindings).ir in
    let field = StringMap.find "field" args in
    match field with
    | Simple { token = Ident name; _ } ->
        {
          ir = FieldAccess { obj; name; data = init_ir_data () };
          new_bindings = StringMap.empty;
        }
    | _ -> failwith "field access must be using an ident"

  let macro = function
    | Function f -> Macro f
    | other -> failwith ("expected a function, got " ^ show other)

  let cmp_fn f : builtin_fn =
    {
      impl =
        dict_fn (fun args ->
            let lhs = StringMap.find "lhs" args in
            let rhs = StringMap.find "rhs" args in
            match (lhs, rhs) with
            | Int32 lhs, Int32 rhs -> Bool (f lhs rhs)
            | _ -> failwith "only int32 is supported for comparisons");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list
                [ ("lhs", (Int32 : value_type)); ("rhs", Int32) ];
          };
      result_type = Bool;
    }

  let dbg : builtin_fn =
    {
      impl =
        (fun value ->
          Log.info (show value ^ " : " ^ show_type (type_of_value value));
          Void);
      arg_type = Any;
      result_type = Void;
    }

  let all =
    StringMap.of_list
      [
        ("any", Type Any);
        ("void", Type Void);
        ("ast", Type Ast);
        ("bool", Type Bool);
        ("int32", Type Int32);
        ("int64", Type Int64);
        ("float32", Type Float32);
        ("float64", Type Float64);
        ("string", Type String);
        ("type_ascribe", BuiltinMacro type_ascribe);
        ("print", BuiltinFn print);
        ("dbg", BuiltinFn dbg);
        ("call", BuiltinMacro call);
        ("then", BuiltinMacro then');
        ("if", BuiltinMacro if');
        ("uplus", BuiltinFn (int32_unary_op (fun x -> x)));
        ("negate", BuiltinFn (int32_unary_op Int32.neg));
        ("add", BuiltinFn (int32_binary_op Int32.add));
        ("sub", BuiltinFn (int32_binary_op Int32.sub));
        ("mul", BuiltinFn (int32_binary_op Int32.mul));
        ("div", BuiltinFn (int32_binary_op Int32.div));
        (* ("mod", BuiltinFn (binary_op ( Stdlib.rem ))); *)
        ("sin", BuiltinFn (float64_fn sin));
        ("cos", BuiltinFn (float64_fn cos));
        ("sqrt", BuiltinFn (float64_fn sqrt));
        ("quote", BuiltinMacro quote);
        ("scope", BuiltinMacro scope);
        ( "type_of_value",
          BuiltinFn
            {
              impl = (fun x -> Type (type_of_value x));
              arg_type = Any;
              result_type = Type;
            } );
        ( "unit",
          BuiltinFn
            { impl = (fun _ -> Void); arg_type = Void; result_type = Void } );
        ("let", BuiltinMacro let');
        ("function_def", BuiltinMacro function_def);
        ("field_access", BuiltinMacro field_access);
        ( "macro",
          BuiltinFn
            (single_arg_fn
               (Fn { arg_type = Ast; result_type = Ast })
               "def" Macro macro) );
        ("less", BuiltinFn (cmp_fn ( < )));
        ("less_or_equal", BuiltinFn (cmp_fn ( <= )));
        ("equal", BuiltinFn (cmp_fn ( = )));
        ("not_equal", BuiltinFn (cmp_fn ( <> )));
        ("greater", BuiltinFn (cmp_fn ( > )));
        ("greater_or_equal", BuiltinFn (cmp_fn ( >= )));
      ]
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
            StringMap.map
              (fun value -> Value value)
              (StringMap.union
                 (fun _key _prev value -> Some value)
                 Builtins.all
                 (StringMap.singleton "Self" (Struct self)));
          syntax = Syntax.empty;
        };
    }
  in
  state

let eval (self : state ref) (s : string) ~(filename : string) : value =
  let filename = Span.Filename filename in
  !self.self.data <- !self.data;
  let tokens = Lexer.parse s filename in
  let ast = Ast.parse !self.data.syntax tokens filename in
  let ast = Ast.map (fun span -> { span; ir = None }) ast in
  let result = eval_ast !self ast None in
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
          locals =
            update_locals !self.data.locals
              (StringMap.map (fun value -> Value value) result.new_bindings);
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
