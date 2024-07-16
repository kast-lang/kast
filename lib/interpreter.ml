open Prelude

type ast_data = { span : Span.span; mutable ir : Ir.t option }
type ast = ast_data Ast.node

type value =
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

and value_type =
  | Any
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
  | Type

and fn_type = { arg_type : value_type; result_type : value_type }

and builtin_fn = {
  name : string;
  impl : value -> value;
  arg_type : value_type;
  result_type : value_type;
}

and builtin_macro = state -> ast StringMap.t -> new_bindings:bool -> compiled

and 'data ir_node =
  | Void of { data : 'data }
  | TypeOf of { captured : state; expr : 'data ir_node; data : 'data }
  | TypeOfValue of { captured : state; expr : 'data ir_node; data : 'data }
  | Dict of { fields : 'data ir_node StringMap.t; data : 'data }
  | Ast of {
      def : Syntax.syntax_def;
      ast_data : ast_data;
      values : 'data ir_node StringMap.t;
      data : 'data;
    }
  | Template of { f : fn; data : 'data }
  | Function of { f : fn; data : 'data }
  | FieldAccess of { obj : 'data ir_node; name : string; data : 'data }
  | Const of { value : value; data : 'data }
  | Binding of { binding : binding; data : 'data }
  | Number of { raw : string; data : 'data }
  | String of { raw : string; value : string; data : 'data }
  | Discard of { value : 'data ir_node; data : 'data }
  | Then of { first : 'data ir_node; second : 'data ir_node; data : 'data }
  | Call of { f : 'data ir_node; args : 'data ir_node; data : 'data }
  (* todo remove? *)
  | Instantiate of {
      captured : state;
      template : 'data ir_node;
      args : 'data ir_node;
      data : 'data;
    }
  | BuiltinFn of { f : builtin_fn; data : 'data }
  | If of {
      cond : 'data ir_node;
      then_case : 'data ir_node;
      else_case : 'data ir_node;
      data : 'data;
    }
  | Let of { pattern : pattern; value : 'data ir_node; data : 'data }

and inference_status = NotYet | InProgress | Done

and ir_data = {
  mutable result_type : value_type option;
  mutable fully_inferred : inference_status;
}

and ir = ir_data ir_node
and fn_result_type = Ast of ast | Actual of value_type

and fn = {
  captured : state;
  args_pattern : ast option;
  result_type : fn_result_type option;
  body : ast;
  mutable compiled : compiled_fn option;
}

and compiled_fn = {
  result_type : ir option;
  captured : state;
  args : pattern;
  body : ir;
}

and pattern =
  | Void
  | Binding of binding
  | Dict of { fields : pattern StringMap.t }

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
  | BuiltinFn { name; _ } -> "builtin_fn " ^ name
  | Template f -> "template"
  | Function f ->
      "function"
      (* ^ (match f.args_pattern with Some ast -> Ast.show ast | None -> "()") *)
      (* ^ " => " ^ Ast.show f.body *)
  | Int32 value -> Int32.to_string value
  | Int64 value -> Int64.to_string value
  | Float64 value -> Float.to_string value
  | Bool value -> Bool.to_string value
  | String value -> "\"" ^ String.escaped value ^ "\""
  | Dict { fields } ->
      "{ "
      ^ StringMap.fold
          (fun name field acc ->
            (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show field)
          fields ""
      ^ " }"
  | Ref value -> show !value
  | Struct _ -> "struct <...>"
  | Type t -> "type " ^ show_type t

and show_fn_type : fn_type -> string =
 fun { arg_type; result_type } ->
  show_type arg_type ^ " -> " ^ show_type result_type

and show_type : value_type -> string = function
  | Any -> "any"
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
  | Template f -> "template"
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

and show_ir : ir -> string = function
  | Void _ -> "void"
  | TypeOf { expr; _ } -> "typeof " ^ show_ir expr
  | TypeOfValue { expr; _ } -> "typeofvalue " ^ show_ir expr
  | Template _ -> "template"
  | Function _ -> "function"
  | Dict { fields; _ } ->
      "{ "
      ^ StringMap.fold
          (fun name field acc ->
            (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show_ir field)
          fields ""
      ^ " }"
  | Number { raw; _ } -> raw
  | Ast _ -> "ast"
  | Const { value; _ } -> "(const " ^ show value ^ ")"
  | FieldAccess { obj; name; _ } -> "(field " ^ show_ir obj ^ " " ^ name ^ ")"
  | BuiltinFn { f = { name; _ }; _ } -> "builtin_fn " ^ name
  | Discard { value; _ } -> "(discard " ^ show_ir value ^ ")"
  | Binding { binding; _ } -> "(binding " ^ binding.name ^ ")"
  | Call { f; args; _ } -> "(call " ^ show_ir f ^ " " ^ show_ir args ^ ")"
  | Instantiate { template; args; _ } ->
      "(instantiate " ^ show_ir template ^ " " ^ show_ir args ^ ")"
  | String { raw; _ } -> raw
  | Then { first; second; _ } ->
      "(then " ^ show_ir first ^ " " ^ show_ir second ^ ")"
  | If { cond; then_case; else_case; _ } ->
      "(if " ^ show_ir cond ^ " " ^ show_ir then_case ^ " " ^ show_ir else_case
      ^ ")"
  | Let { pattern; value; _ } ->
      "(let " ^ show_pattern pattern ^ " " ^ show_ir value ^ ")"

and show_pattern : pattern -> string = function
  | Void -> "()"
  | Binding binding -> binding.name
  | Dict { fields } ->
      "{ "
      ^ StringMap.fold
          (fun name field acc ->
            (if acc = "" then "" else acc ^ ", ")
            ^ name ^ ": " ^ show_pattern field)
          fields ""
      ^ " }"

and type_check_fn (expected : fn_type) (actual : fn_type) : bool =
  let { arg_type = expected_arg; result_type = expected_result } = expected in
  let { arg_type = actual_arg; result_type = actual_result } = actual in
  type_check expected_arg actual_arg && type_check expected_result actual_result

and type_check (expected : value_type) (actual : value_type) : bool =
  match (expected, actual) with
  | Any, _ -> true
  | _, Never -> true
  | Never, _ -> false
  | Ast, Ast -> true
  | Ast, _ -> false
  | Void, Void -> true
  | Void, _ -> false
  | Bool, Bool -> true
  | Bool, _ -> false
  | Int32, Int32 -> true
  | Int32, _ -> false
  | Int64, Int64 -> true
  | Int64, _ -> false
  | Float32, Float32 -> true
  | Float32, _ -> false
  | Float64, Float64 -> true
  | Float64, _ -> false
  | String, String -> true
  | String, _ -> false
  | Fn expected, Fn actual -> type_check_fn expected actual
  | Fn _, _ -> false
  | Macro expected, Macro actual -> type_check_fn expected actual
  | Macro _, _ -> false
  | Dict { fields = expected_fields }, Dict { fields = actual_fields } ->
      StringMap.equal type_check expected_fields actual_fields
  | Dict _, _ -> false
  | Type, _ -> true
  | BuiltinMacro, BuiltinMacro -> true
  | BuiltinMacro, _ -> false
  | Template expected, Template actual -> true
  | Template _, _ -> false

let rec type_of_fn : fn -> fn_type =
 fun f ->
  Log.trace "getting type of fun";
  let compiled = ensure_compiled f in
  let result_type = fully_infer_types compiled.body None in
  { result_type; arg_type = pattern_type_sure compiled.args None }

and type_of_value : value -> value_type = function
  | Ast _ -> Ast
  | Void -> Void
  | BuiltinMacro _ -> BuiltinMacro
  | BuiltinFn { arg_type; result_type; _ } -> Fn { arg_type; result_type }
  | Template f ->
      Template
        {
          args_pattern = f.args_pattern;
          result_type = None;
          captured = f.captured;
          body =
            Complex
              {
                def =
                  {
                    name = "typeofvalue";
                    assoc = Left;
                    priority = 0;
                    parts = [];
                  };
                values = StringMap.singleton "expr" f.body;
                data = Ast.data f.body;
              };
          compiled = None;
        }
  | Macro f -> Macro (type_of_fn f)
  | Function f -> Fn (type_of_fn f)
  | Int32 _ -> Int32
  | Int64 _ -> Int64
  | Float64 _ -> Float64
  | Bool _ -> Bool
  | String _ -> String
  | Dict { fields } -> Dict { fields = StringMap.map type_of_value fields }
  | Ref _ -> failwith "todo"
  | Struct _ -> failwith "todo"
  | Type _ -> Type

and ir_data : 'data. 'data ir_node -> 'data = function
  | Void { data; _ }
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

and init_ir_data () : ir_data = { result_type = None; fully_inferred = NotYet }

and just_value value expected_type =
  (match expected_type with
  | Some Any -> ()
  | Some expected_type ->
      let actual_type = type_of_value value in
      if not (type_check expected_type actual_type) then
        failwith
          ("value is of wrong type (expected " ^ show_type expected_type
         ^ ", got " ^ show_type actual_type ^ ")")
  | None -> ());
  { value; new_bindings = StringMap.empty }

and update_locals =
  StringMap.union (fun _name _prev new_value -> Some new_value)

(* this is lexeme val from_string : (Cparser.token, 'a) MenhirLib.Convert.traditional -> string -> 'a *)

and pattern_match_opt (pattern : pattern) (value : value) :
    value StringMap.t option =
  match pattern with
  | Void -> ( match value with Void -> Some StringMap.empty | _ -> None)
  | Binding { name; _ } -> Some (StringMap.singleton name value)
  | Dict { fields = field_patterns } -> (
      match value with
      | Dict { fields = field_values } ->
          let fields =
            StringMap.merge
              (fun name pattern value ->
                match (pattern, value) with
                | Some pattern, Some value -> Some (pattern, value)
                | Some _pattern, None ->
                    failwith (name ^ " is not a field in value")
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
  | Some ast ->
      let rec ir_to_pattern (ir : ir) : pattern =
        match ir with
        | Void _ -> Void
        | TypeOfValue _ -> failwith "todo pattern typeofvalue"
        | TypeOf _ -> failwith "todo pattern typeof"
        | Instantiate _ -> failwith "todo patt insta"
        | Template _ -> failwith "todo patt templ"
        | Function _ -> failwith "todo patt fun"
        | Dict { fields; _ } ->
            Dict { fields = StringMap.map ir_to_pattern fields }
        | Ast _ -> failwith "todo pattern Ast"
        | FieldAccess _ -> failwith "todo pattern FieldAccess"
        | Const { value = Type Void; _ } -> Void
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
        | Let _ -> failwith "todo pattern Let"
      in
      ir_to_pattern (compile_ast self ast ~new_bindings:true).ir

and pattern_type (pattern : pattern) (expected_type : value_type option) :
    value_type option =
  match pattern with
  | Void -> Some Void
  | Binding binding -> (
      match (expected_type, binding.value_type) with
      | Some expected, None ->
          binding.value_type <- Some expected;
          Some expected
      | Some expected, Some specified ->
          if not (type_check expected specified) then
            failwith "pattern type wrong";
          Some expected
      | None, _ -> binding.value_type)
  | Dict { fields } -> (
      match expected_type with
      | Some (Dict { fields = expected_field_types }) ->
          Some
            (Dict
               {
                 fields =
                   StringMap.merge
                     (fun name field expected_type ->
                       match (field, expected_type) with
                       | Some field, Some expected_type ->
                           Some
                             (Option.get
                                (pattern_type field (Some expected_type)))
                       | Some _field, None ->
                           failwith
                             ("pattern specifies field " ^ name
                            ^ " which was not expected")
                       | None, Some _expected_type ->
                           failwith ("pattern does not specify field " ^ name)
                       | None, None -> failwith "unreachable")
                     fields expected_field_types;
               })
      | Some expected ->
          failwith ("pattern is a dict,  but expected " ^ show_type expected)
      | None ->
          Some
            (Dict
               {
                 fields =
                   StringMap.map
                     (fun field ->
                       match pattern_type field None with
                       | Some typ -> typ
                       | None -> failwith "todo partially inferred types")
                     fields;
               }))

and pattern_type_sure (pattern : pattern) (expected_type : value_type option) :
    value_type =
  match pattern_type pattern expected_type with
  | Some typ -> typ
  | None ->
      failwith
        ("todo pattern type could not be figured out: " ^ show_pattern pattern)

and pattern_bindings (pattern : pattern) : binding StringMap.t =
  match pattern with
  | Void -> StringMap.empty
  | Binding binding -> StringMap.singleton binding.name binding
  | Dict { fields } ->
      StringMap.fold
        (fun _name field acc ->
          StringMap.union
            (fun name _old _new ->
              failwith (name ^ " appears multiple times in pattern"))
            acc (pattern_bindings field))
        fields StringMap.empty

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
          let values =
            Dict { fields = StringMap.map (fun x -> Ast x) values }
          in
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
          if not (type_check expected_type actual_type) then
            failwith
              ("expected " ^ show_type expected_type ^ ", got "
             ^ show_type actual_type)
      | None -> ir_data.result_type <- Some expected_type)
  | None -> ()

and maybe_infer_types (ir : ir) (expected_type : value_type option)
    ~(full : bool) : value_type option =
  let ir_data = ir_data ir in
  forward_expected_type ir expected_type;
  let need_to_infer =
    Option.is_none ir_data.result_type
    || (full && ir_data.fully_inferred = NotYet)
  in
  if need_to_infer then (
    if full then ir_data.fully_inferred <- InProgress;
    let inferred : value_type option =
      match ir with
      | Void _ -> Some Void
      | TypeOf { captured; expr; _ } -> Some Type
      | TypeOfValue { captured; expr; _ } -> Some Type
      | BuiltinFn { f = { arg_type; result_type; _ }; _ } ->
          Some (Fn { arg_type; result_type })
      | Function { f; _ } | Template { f; _ } ->
          if full then (
            let compiled = ensure_compiled f in
            ignore (pattern_type_sure compiled.args None);
            ignore (fully_infer_types compiled.body None));
          Option.map
            (fun compiled ->
              Fn
                {
                  (* todo *)
                  arg_type = Option.get (pattern_type compiled.args None);
                  result_type =
                    Option.get (maybe_infer_types compiled.body None ~full);
                })
            f.compiled
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
                       Option.get
                         (maybe_infer_types value
                            (StringMap.find_opt name field_types)
                            ~full))
                     fields;
               })
      | Ast { values; _ } ->
          if full then
            StringMap.iter
              (fun _name value ->
                ignore (fully_infer_types value (Some (Ast : value_type))))
              values;
          Some Ast
      | FieldAccess { obj; name; _ } -> (
          match fully_infer_types obj None with
          | Dict { fields } -> Some (StringMap.find name fields)
          | _ -> failwith "todo")
      | Const { value = Function f; _ } -> (
          if full then ignore (ensure_compiled f);
          match f.compiled with
          | None -> None
          | Some compiled ->
              Some
                (Fn
                   {
                     arg_type = pattern_type_sure compiled.args None;
                     result_type = fully_infer_types compiled.body None;
                   }))
      | Const { value; _ } ->
          Log.trace ("inferring type of " ^ show value);
          Some (type_of_value value)
      | Binding { binding; _ } -> (
          match binding.value_type with
          | Some typ -> Some typ
          | None ->
              binding.value_type <- expected_type;
              expected_type)
      | Number _ -> expected_type (* todo check? *)
      | String _ -> Some String
      | Discard { value; _ } ->
          if full then ignore (fully_infer_types value None);
          Some Void
      | Then { first; second; _ } ->
          if full then ignore (fully_infer_types first None);
          maybe_infer_types second expected_type ~full
      | Instantiate { captured; template; args; _ } ->
          let result =
            match maybe_infer_types template None ~full with
            | Some (Template f) ->
                let compiled = ensure_compiled f in
                let args =
                  eval_ir captured args (pattern_type compiled.args None)
                in
                Log.trace
                  ("instantiating type inf " ^ show_pattern compiled.args
                 ^ " => " ^ show_ir compiled.body);
                Some (value_to_type (call_compiled compiled args.value))
            | Some Any -> Some Any
            | Some _ -> failwith "not template"
            | None -> None
          in
          Log.trace
            ("type of instantiating " ^ show_ir template ^ " "
           ^ Bool.to_string full ^ " = "
            ^ match result with Some t -> show_type t | None -> "<unknown>");
          result
      | Call { f; args; _ } -> (
          Log.trace ("trying to call " ^ show_ir f);
          match maybe_infer_types f None ~full with
          | Some (Fn f | Macro f) ->
              let { arg_type; result_type; _ } = f in
              if full then ignore (fully_infer_types args (Some arg_type));
              Some result_type
          | Some BuiltinMacro -> None
          | Some Any -> None
          | Some typ -> failwith (show_type typ ^ " is not fun")
          | None -> None)
      | If { cond; then_case; else_case; _ } ->
          if full then ignore (fully_infer_types cond (Some Bool));
          let then_type = maybe_infer_types then_case None ~full:false in
          let else_type = maybe_infer_types else_case then_type ~full in
          let common_type =
            match (then_type, else_type) with
            | Some then_case, Some else_case ->
                (* todo same_types instead of type_check? *)
                if not (type_check then_case else_case) then
                  failwith
                    ("then and else diff types: " ^ show_type then_case
                   ^ " and " ^ show_type else_case);
                Some then_case
            | Some case, None | None, Some case -> Some case
            | None, None -> None
          in
          if full then ignore (fully_infer_types then_case common_type);
          common_type
      | Let { pattern; value; _ } ->
          (if full then
             let typ = fully_infer_types value (pattern_type pattern None) in
             ignore (pattern_type_sure pattern (Some typ)));
          Some Void
    in
    if full && Option.is_none inferred then
      failwith ("failed to infer type of " ^ show_ir ir);
    if full then ir_data.fully_inferred <- Done;
    match inferred with
    | Some actual_type -> (
        match ir_data.result_type with
        | None -> ir_data.result_type <- Some actual_type
        | Some expected_type ->
            if not (type_check expected_type actual_type) then
              failwith
                (show_ir ir ^ " expected " ^ show_type expected_type ^ ", got "
               ^ show_type actual_type))
    | None -> ());
  ir_data.result_type

and fully_infer_types (ir : ir) (expected_type : value_type option) : value_type
    =
  Option.get (maybe_infer_types ir expected_type ~full:true)

and log_state (self : state) : unit =
  let log = Log.never in
  log "locals:";
  StringMap.iter
    (fun name local ->
      log
        (name ^ " = "
        ^
        match local with
        | Value value -> show value
        | Binding binding -> "binding " ^ binding.name))
    self.data.locals

and show_or : 'a. string -> ('a -> string) -> 'a option -> string =
 fun default f opt -> match opt with Some value -> f value | None -> default

and show_local : state_local -> string = function
  | Value value -> show value
  | Binding binding -> "binding " ^ binding.name

and eval_ir (self : state) (ir : ir) (expected_type : value_type option) :
    evaled =
  Log.trace
    ("evaluating " ^ show_ir ir ^ " as "
    ^ show_or "<unknown>" show_type expected_type);
  log_state self;
  Log.trace
    ("picked = " ^ show_or "<none>" show_local (get_local_opt self "picked"));
  let result_type = maybe_infer_types ir expected_type ~full:false in
  Log.trace
    ("result type of " ^ show_ir ir ^ " inferred as "
    ^ show_or "<unknown>" show_type result_type);
  (* forward_expected_type ir expected_type; *)
  (* let result_type = (ir_data ir).result_type in *)
  let result =
    match ir with
    | Void _ -> just_value Void result_type
    | TypeOf { captured; expr; _ } ->
        just_value (Type (fully_infer_types expr (Some Type))) (Some Type)
    | TypeOfValue { captured; expr; _ } ->
        just_value
          (Type (type_of_value (eval_ir self expr (Some Any)).value))
          (Some Type)
    | BuiltinFn { f; _ } -> just_value (BuiltinFn f) result_type
    | Dict { fields; _ } ->
        let field_types =
          match result_type with
          | Some (Dict { fields }) -> fields
          | Some Type | None -> StringMap.empty (* todo None? *)
          | Some t -> failwith (show_type t ^ " is not a dict")
        in
        just_value
          (Dict
             {
               fields =
                 StringMap.mapi
                   (fun name value ->
                     (eval_ir self value (StringMap.find_opt name field_types))
                       .value)
                   fields;
             })
          result_type
    | Template { f; _ } ->
        just_value
          (Template { f with captured = self; compiled = None })
          result_type
    | Function { f; _ } ->
        just_value
          (Function { f with captured = self; compiled = None })
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
    | Then { first; second; _ } ->
        let first = eval_ir self first None in
        discard first.value;
        let result =
          eval_ir
            {
              self with
              data =
                {
                  self.data with
                  locals =
                    update_locals self.data.locals
                      (StringMap.mapi
                         (fun _name value ->
                           Log.trace (_name ^ " = " ^ show value);
                           Value value)
                         first.new_bindings);
                };
            }
            second result_type
        in
        {
          result with
          new_bindings =
            StringMap.union
              (fun _name _a b -> Some b)
              first.new_bindings result.new_bindings;
        }
    | Instantiate { template; args; _ } ->
        let template = (eval_ir self template None).value in
        Log.trace ("instantiating " ^ show template);
        let f, args_type =
          match template with
          | Template f ->
              let compiled = ensure_compiled f in
              ( call_compiled compiled,
                pattern_type compiled.args (ir_data args).result_type )
          | _ -> failwith "not a template"
        in
        Log.trace
          ("args_type = "
          ^
          match args_type with
          | Some args_type -> show_type args_type
          | None -> "<unknown>");
        let args = (eval_ir self args args_type).value in
        (* todo memoization *)
        just_value (f args) result_type
    | Call { f; args; _ } ->
        let f = (eval_ir self f None).value in
        Log.trace ("calling " ^ show f);
        let f, args_type =
          match f with
          | BuiltinFn f -> (f.impl, Some f.arg_type)
          | Function f ->
              let compiled = ensure_compiled f in
              ( call_compiled compiled,
                pattern_type compiled.args (ir_data args).result_type )
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
          ("evaling if, common type = "
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
          new_bindings =
            pattern_match pattern
              (eval_ir self value
                 (pattern_type pattern (ir_data value).result_type))
                .value;
        }
  in
  Log.trace
    ("evaluated " ^ show_ir ir ^ " = " ^ show result.value ^ " as "
    ^ show_or "<unknown>" show_type result_type);
  match result_type with
  | Some Type -> { result with value = Type (value_to_type result.value) }
  | _ -> result

and value_to_type : value -> value_type = function
  | Type t -> t
  | Dict { fields } -> Dict { fields = StringMap.map value_to_type fields }
  | Template f -> Template f
  | other -> failwith (show other ^ " is not a type")

and compile_and_call (f : fn) (args : value) : value =
  call_compiled (ensure_compiled f) args

and ensure_compiled (f : fn) : compiled_fn =
  if Option.is_none f.compiled then (
    Log.trace ("compiling " ^ Ast.show f.body);
    f.compiled <-
      (let args = compile_pattern f.captured f.args_pattern in
       let captured =
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
       in
       Some
         {
           captured = f.captured;
           args;
           result_type =
             (match f.result_type with
             | None -> None
             | Some (Actual t) ->
                 Some (Const { value = Type t; data = init_ir_data () })
             | Some (Ast ast) ->
                 Some (compile_ast captured ast ~new_bindings:false).ir);
           body = (compile_ast captured f.body ~new_bindings:false).ir;
         }));
  Option.get f.compiled

and call_compiled (f : compiled_fn) (args : value) : value =
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
                   Value value)
                 (pattern_match f.args args));
        };
    }
  in
  (* todo lazy *)
  let expected_type =
    Option.map
      (fun ir -> value_to_type (eval_ir f.captured ir (Some Type)).value)
      f.result_type
  in
  Log.trace
    ("calling " ^ show_ir f.body ^ " with " ^ show args ^ " expecting "
    ^ show_or "<unknown>" show_type expected_type);
  (eval_ir captured f.body expected_type).value

and discard : value -> unit = function
  | Void -> ()
  | that -> failwith ("only void can be discarded (discarded " ^ show that ^ ")")

and get_field obj field =
  match obj with
  | Dict { fields = dict } -> StringMap.find field dict
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

  let instantiate_template : builtin_macro =
   fun self args ~new_bindings ->
    let template =
      compile_ast self (StringMap.find "template" args) ~new_bindings
    in
    let args = compile_ast self (StringMap.find "args" args) ~new_bindings in
    {
      ir =
        Instantiate
          {
            captured = self;
            template = template.ir;
            args = args.ir;
            data = init_ir_data ();
          };
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
    let b =
      match StringMap.find_opt "b" args with
      | Some b -> compile_ast self_with_new_bindings b ~new_bindings
      | None ->
          {
            ir = Void { data = init_ir_data () };
            new_bindings = StringMap.empty;
          }
    in
    {
      ir = Then { first = a.ir; second = b.ir; data = init_ir_data () };
      new_bindings = update_locals a.new_bindings b.new_bindings;
    }

  let print : builtin_fn =
    {
      name = "print";
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

  let dict_fn f = function
    | Dict { fields = args } -> f args
    | _ -> failwith "expected dict"

  let int32_binary_op_with name lhs rhs f : builtin_fn =
    {
      name = "int32 binary " ^ name;
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

  let float64_fn name f : builtin_fn =
    {
      name;
      impl =
        (function
        | Float64 value -> Float64 (f value) | _ -> failwith "only floats");
      arg_type = Float64;
      result_type = Float64;
    }

  let int32_fn name f : builtin_fn =
    {
      name;
      impl =
        (function
        | Int32 value -> Int32 (f value) | _ -> failwith "only floats");
      arg_type = Int32;
      result_type = Int32;
    }

  let single_arg_fn fn_name arg_type arg_name result_type f : builtin_fn =
    {
      name = fn_name;
      impl =
        dict_fn (fun args ->
            let value = StringMap.find arg_name args in
            f value);
      arg_type = Dict { fields = StringMap.singleton arg_name arg_type };
      result_type;
    }

  let float64_macro fn_name arg_name f =
    let f = float64_fn fn_name f in
    single_arg_fn fn_name Float64 arg_name Float64 f.impl

  let int32_macro fn_name arg_name f =
    let f = int32_fn fn_name f in
    single_arg_fn fn_name Int32 arg_name Int32 f.impl

  let int32_unary_op name = int32_macro ("int32 unary " ^ name) "x"

  let scope : builtin_macro =
   fun self args ~new_bindings ->
    let e = StringMap.find "e" args in
    {
      ir = (compile_ast self e ~new_bindings).ir;
      new_bindings = StringMap.empty;
    }

  let comptime : builtin_macro =
   fun self args ~new_bindings ->
    let value = StringMap.find "value" args in
    {
      ir =
        Const
          { value = (eval_ast self value None).value; data = init_ir_data () };
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

  let const_let : builtin_macro =
   fun self args ~new_bindings ->
    let ir = (let' self args ~new_bindings).ir in
    let evaled = eval_ir self ir (Some Void) in
    {
      ir;
      new_bindings =
        StringMap.map (fun value -> Value value) evaled.new_bindings;
    }

  let template_def : builtin_macro =
   fun self args ~new_bindings ->
    let where = StringMap.find_opt "where" args in
    if Option.is_some where then failwith "todo where clauses";
    let def = StringMap.find "def" args in
    let args = StringMap.find "args" args in
    {
      ir =
        Template
          {
            f =
              {
                captured = self;
                result_type = Some (Actual Any);
                args_pattern = Some args;
                body = def;
                compiled = None;
              };
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let function_def : builtin_macro =
   fun self args ~new_bindings ->
    let args_pattern = StringMap.find_opt "args" args in
    let result_type = StringMap.find_opt "result_type" args in
    let body = StringMap.find "body" args in
    {
      ir =
        Function
          {
            f =
              {
                result_type =
                  Option.map (fun t : fn_result_type -> Ast t) result_type;
                captured = self;
                args_pattern;
                body;
                compiled = None;
              };
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

  let field : builtin_macro =
   fun self args ~new_bindings ->
    let name = StringMap.find "name" args in
    let value = StringMap.find "value" args in
    let name =
      match name with
      | Ast.Simple { token = Ident name; _ } -> name
      | _ -> failwith "field name must be an ident"
    in
    {
      ir =
        Dict
          {
            fields =
              StringMap.singleton name (compile_ast self value ~new_bindings).ir;
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let typeof : builtin_macro =
   fun self args ~new_bindings ->
    let expr = StringMap.find "expr" args in
    {
      ir =
        TypeOf
          {
            captured = self;
            expr = (compile_ast self expr ~new_bindings).ir;
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let typeofvalue : builtin_macro =
   fun self args ~new_bindings ->
    let expr = StringMap.find "expr" args in
    {
      ir =
        TypeOfValue
          {
            captured = self;
            expr = (compile_ast self expr ~new_bindings).ir;
            data = init_ir_data ();
          };
      new_bindings = StringMap.empty;
    }

  let tuple : builtin_macro =
   fun self args ~new_bindings ->
    let a = StringMap.find "a" args in
    let a = compile_ast self a ~new_bindings in
    let fields : ir -> _ = function
      | Dict { fields; _ } -> fields
      | _ -> failwith "expected a dict"
    in
    match StringMap.find_opt "b" args with
    | Some b ->
        let b = compile_ast self b ~new_bindings in
        {
          ir =
            Dict
              {
                fields =
                  StringMap.union
                    (fun name _a _b ->
                      failwith (name ^ " is specified multiple times"))
                    (fields a.ir) (fields b.ir);
                data = init_ir_data ();
              };
          new_bindings = StringMap.empty;
        }
    | None ->
        {
          ir = Dict { fields = fields a.ir; data = init_ir_data () };
          new_bindings = StringMap.empty;
        }

  let macro = function
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
      name = "dbg";
      impl =
        (fun value ->
          print_endline (show value ^ " : " ^ show_type (type_of_value value));
          Void);
      arg_type = Any;
      result_type = Void;
    }

  let get_int32 : value -> int32 = function
    | Int32 value -> value
    | _ -> failwith "expected int32"

  let get_float64 : value -> float = function
    | Float64 value -> value
    | _ -> failwith "expected float64"

  let get_type : value -> value_type = function
    | Type value_type -> value_type
    | _ -> failwith "expected type"

  let function_type : builtin_fn =
    {
      name = "function_type";
      impl =
        (function
        | Dict { fields = args } ->
            let arg_type = get_type (StringMap.find "arg" args) in
            let result_type = get_type (StringMap.find "result" args) in
            let result = Type (Fn { arg_type; result_type }) in
            Log.trace (show result);
            result
        | _ -> failwith "expected dict");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list
                [ ("arg", (Type : value_type)); ("result", Type) ];
          };
      result_type = Type;
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
                 (Random.int32 (Int32.sub (Int32.add max (Int32.of_int 1)) min)))
        | _ -> failwith "expected dict");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list
                [ ("min", (Int32 : value_type)); ("max", Int32) ];
          };
      result_type = Int32;
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
        | _ -> failwith "expected dict");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list
                [ ("min", (Float64 : value_type)); ("max", Float64) ];
          };
      result_type = Float64;
    }

  let panic : builtin_fn =
    {
      name = "panic";
      impl =
        (function
        | String s -> failwith s
        | _ -> failwith "panicked with not a string kekw");
      arg_type = String;
      result_type = Never;
    }

  let is_same_type : builtin_fn =
    {
      name = "is_same_type";
      impl =
        (function
        | Dict { fields } ->
            let a = get_type (StringMap.find "a" fields) in
            let b = get_type (StringMap.find "b" fields) in
            let result = a = b in
            Log.trace
              ("is_same_type " ^ show_type a ^ ", " ^ show_type b ^ " = "
             ^ Bool.to_string result);
            Bool result
        | _ -> failwith "expected dict");
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list [ ("a", (Type : value_type)); ("b", Type) ];
          };
      result_type = Bool;
    }

  let all =
    StringMap.of_list
      [
        ("typeof", BuiltinMacro typeof);
        ("typeofvalue", BuiltinMacro typeofvalue);
        ("any", Type Any);
        ("void", Type Void);
        ("ast", Type Ast);
        ("bool", Type Bool);
        ("true", Bool true);
        ("false", Bool false);
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
        ("uplus", BuiltinFn (int32_unary_op "+" (fun x -> x)));
        ("negate", BuiltinFn (int32_unary_op "-" Int32.neg));
        ("add", BuiltinFn (int32_binary_op "+" Int32.add));
        ("sub", BuiltinFn (int32_binary_op "-" Int32.sub));
        ("mul", BuiltinFn (int32_binary_op "*" Int32.mul));
        ("div", BuiltinFn (int32_binary_op "/" Int32.div));
        (* ("mod", BuiltinFn (binary_op ( Stdlib.rem ))); *)
        ("sin", BuiltinFn (float64_fn "sin" sin));
        ("cos", BuiltinFn (float64_fn "cos" cos));
        ("sqrt", BuiltinFn (float64_fn "sqrt" sqrt));
        ("quote", BuiltinMacro quote);
        ("scope", BuiltinMacro scope);
        ( "type_of_value",
          BuiltinFn
            {
              name = "type_of_value";
              impl = (fun x -> Type (type_of_value x));
              arg_type = Any;
              result_type = Type;
            } );
        ( "unit",
          BuiltinFn
            {
              name = "unit(void)";
              impl = (fun _ -> Void);
              arg_type = Dict { fields = StringMap.empty };
              result_type = Void;
            } );
        ("let", BuiltinMacro let');
        ("const_let", BuiltinMacro const_let);
        ("function_def", BuiltinMacro function_def);
        ("field_access", BuiltinMacro field_access);
        ( "macro",
          BuiltinFn
            (single_arg_fn "macro" Any
               (* todo { _anyfield: ast } -> ast #  (Fn { arg_type = Ast; result_type = Ast }) *)
               "def" Any macro) );
        ("less", BuiltinFn (cmp_fn "<" ( < )));
        ("less_or_equal", BuiltinFn (cmp_fn "<=" ( <= )));
        ("equal", BuiltinFn (cmp_fn "==" ( = )));
        ("not_equal", BuiltinFn (cmp_fn "!=" ( <> )));
        ("greater", BuiltinFn (cmp_fn ">" ( > )));
        ("greater_or_equal", BuiltinFn (cmp_fn ">=" ( >= )));
        ("field", BuiltinMacro field);
        ("tuple", BuiltinMacro tuple);
        ("function_type", BuiltinFn function_type);
        ("random_int32", BuiltinFn random_int32);
        ("random_float64", BuiltinFn random_float64);
        ("type", Type Type);
        ( "string_to_int32",
          BuiltinFn
            {
              name = "string_to_int32";
              impl =
                (function
                | String s -> Int32 (Int32.of_string s)
                | _ -> failwith "expected string");
              arg_type = String;
              result_type = Int32;
            } );
        ( "input",
          BuiltinFn
            {
              name = "input";
              impl =
                (function
                | Void -> String (read_line ()) | _ -> failwith "expected void");
              arg_type = Void;
              result_type = String;
            } );
        ("template_def", BuiltinMacro template_def);
        ("instantiate_template", BuiltinMacro instantiate_template);
        ("is_same_type", BuiltinFn is_same_type);
        ("panic", BuiltinFn panic);
        ("comptime", BuiltinMacro comptime);
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
  Log.trace (Ast.show ast);
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
