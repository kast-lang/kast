open Prelude

type ast_data = { span : Span.span }
type ast = ast_data Ast.node
type id = Id.t

type value =
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
  | Any
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
  | SpecificType of value_type
  | Type
  | Var of id

and type_var_map = value_type Id.Map.t

and fn_type = {
  arg_type : value_type;
  contexts : contexts_type;
  result_type : value_type;
}

and builtin_fn = {
  name : string;
  impl : value -> value;
  arg_type : value_type;
  result_type : value_type;
  contexts : contexts_type;
}

and builtin_macro = {
  name : string;
  impl : state -> ast StringMap.t -> new_bindings:bool -> expanded_macro;
}

and 'data match_branch = { pattern : pattern; body : 'data ir_node }

and 'data ir_node =
  | Void of { data : 'data }
  | Struct of { body : 'data ir_node; data : 'data }
  | CreateImpl of {
      captured : state;
      ty : 'data ir_node;
      trait : 'data ir_node;
      impl : 'data ir_node;
      data : 'data;
    }
  | GetImpl of {
      captured : state;
      ty : 'data ir_node;
      trait : 'data ir_node;
      data : 'data;
    }
  | CheckImpl of {
      captured : state;
      ty : 'data ir_node;
      trait : 'data ir_node;
      data : 'data;
    }
  | Match of {
      value : 'data ir_node;
      branches : 'data match_branch list;
      data : 'data;
    }
  | NewType of { def : pattern; data : 'data }
  | Scope of { expr : 'data ir_node; data : 'data }
  | OneOf of { variants : 'data ir_node option StringMap.t; data : 'data }
  | TypeOf of { captured : state; expr : 'data ir_node; data : 'data }
  | TypeOfValue of { captured : state; expr : 'data ir_node; data : 'data }
  | Dict of { fields : 'data ir_node StringMap.t; data : 'data }
  | Unwinding of { f : 'data ir_node; data : 'data }
  | WithContext of {
      new_context : 'data ir_node;
      expr : 'data ir_node;
      data : 'data;
    }
  | CurrentContext of { context_type : value_type; data : 'data }
  | Ast of {
      def : Syntax.syntax_def;
      ast_data : ast_data;
      values : 'data ir_node StringMap.t;
      data : 'data;
    }
  | Template of { f : fn; data : 'data }
  | Function of { f : fn; data : 'data }
  | FieldAccess of {
      obj : 'data ir_node;
      name : string;
      default_value : 'data ir_node option;
      data : 'data;
    }
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

and type_inference_data = {
  mutable result_type : value_type option;
  mutable fully_inferred : inference_status;
}

and ir_data = type_inference_data
and ir = ir_data ir_node
and fn_result_type = Ast of ast | Actual of value_type

and fn = {
  where_clause : ast option;
  captured : state;
  args_pattern : ast option;
  result_type : fn_result_type option;
  contexts : ast option;
  body : ast;
  mutable compiled : compiled_fn option;
}

and compiled_fn = {
  (* I will change this later (to value_type option i think) *)
  where_clause : ir;
  result_type : ir option;
  contexts : contexts_type;
  captured : state;
  args : pattern;
  body : ir;
}

and 'data pattern_node =
  | Void of { data : 'data }
  | Binding of { binding : binding; data : 'data }
  | Dict of { fields : 'data pattern_node StringMap.t; data : 'data }
  | Variant of {
      name : string;
      value : 'data pattern_node option;
      data : 'data;
    }
  | Union of { a : 'data pattern_node; b : 'data pattern_node; data : 'data }
  | Typed of { pattern : 'data pattern_node; typ : value_type; data : 'data }

and pattern_data = type_inference_data
and pattern = pattern_data pattern_node
and expanded_macro = Compiled of compiled | Pattern of pattern
and evaled = { value : value; new_bindings : value StringMap.t }
and compiled = { ir : ir; new_bindings : state_local StringMap.t }
and struct' = { parent : struct' option; mutable data : state_data }
and contexts = (value_type, value list) Hashtbl.t
and contexts_type = (value_type, int) Hashtbl.t
and state = { self : struct'; data : state_data; contexts : contexts }
and state_data = { locals : state_local StringMap.t; syntax : Syntax.syntax }
and state_local = Value of value | Binding of binding
and binding = { name : string; mutable value_type : value_type option }
and impl_def = { trait : value_type; ty : value_type }

exception Unwind of id * value

let empty_contexts : contexts = Hashtbl.create 0
let empty_contexts_type : contexts_type = Hashtbl.create 0
let default_contexts_type : contexts_type = empty_contexts_type
let trait_impls : (impl_def, value) Hashtbl.t = Hashtbl.create 0
let empty_type_var_map () : type_var_map ref = ref Id.Map.empty

let rec show = function
  | Ast ast -> "`(" ^ Ast.show ast ^ ")"
  | Variant { name; value; _ } ->
      name ^ show_or "" (fun value -> " " ^ show value) value
  | UnwindToken id -> "unwind token " ^ Id.show id
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
  | Any -> "any"
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
  | SpecificType t -> "typeof " ^ show_type t
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
  | Var id -> "var " ^ Id.show id

and show_ir : ir -> string = function
  | Void _ -> "void"
  | Struct { body; _ } -> "struct (" ^ show_ir body ^ ")"
  | CreateImpl { trait; ty; impl; _ } ->
      "impl " ^ show_ir trait ^ " for " ^ show_ir ty ^ " as " ^ show_ir impl
  | GetImpl { trait; ty; _ } -> show_ir ty ^ " as " ^ show_ir trait
  | CheckImpl { trait; ty; _ } -> show_ir ty ^ " impls " ^ show_ir trait
  | Match { value; branches; _ } ->
      "match " ^ show_ir value ^ " ("
      (* why can not we just have the for? *)
      ^ List.fold_left
          (fun acc branch ->
            acc ^ " | "
            ^ show_pattern branch.pattern
            ^ " => " ^ show_ir branch.body)
          "" branches
      ^ ")"
  | OneOf { variants; _ } ->
      StringMap.fold
        (fun name variant acc ->
          (if acc = "" then "" else acc ^ " | ")
          ^ name
          ^
          match variant with
          | Some variant -> " of " ^ show_ir variant
          | None -> "")
        variants ""
  | NewType { def; _ } -> "newtype " ^ show_pattern def
  | Scope { expr; _ } -> "(" ^ show_ir expr ^ ")"
  | TypeOf { expr; _ } -> "typeof " ^ show_ir expr
  | TypeOfValue { expr; _ } -> "typeofvalue " ^ show_ir expr
  | Template _ -> "template"
  | Function _ -> "function"
  | Unwinding { f; _ } -> "unwinding " ^ show_ir f
  | WithContext { new_context; expr; _ } ->
      "with " ^ show_ir new_context ^ " (" ^ show_ir expr ^ ")"
  | CurrentContext { context_type; _ } ->
      "current_context " ^ show_type context_type
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
  | Void _ -> "()"
  | Union { a; b; _ } -> show_pattern a ^ " | " ^ show_pattern b
  | Binding { binding; _ } -> binding.name
  | Typed { pattern; typ; _ } -> show_pattern pattern ^ " :: " ^ show_type typ
  | Variant { name; value; _ } ->
      name ^ show_or "" (fun value -> " " ^ show_pattern value) value
  | Dict { fields; _ } ->
      "{ "
      ^ StringMap.fold
          (fun name field acc ->
            (if acc = "" then "" else acc ^ ", ")
            ^ name ^ ": " ^ show_pattern field)
          fields ""
      ^ " }"

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

and type_check_fn (expected : fn_type) (actual : fn_type)
    (variables : type_var_map ref) : bool =
  let {
    arg_type = expected_arg;
    contexts = expected_contexts;
    result_type = expected_result;
  } =
    expected
  in
  let {
    arg_type = actual_arg;
    contexts = actual_contexts;
    result_type = actual_result;
  } =
    actual
  in
  type_check expected_arg actual_arg variables
  && type_check expected_result actual_result variables
  && type_check_contexts expected_contexts actual_contexts variables

and type_check (expected : value_type) (actual : value_type)
    (variables : type_var_map ref) : bool =
  match (expected, actual) with
  | Any, _ -> true
  | _, Never -> true
  | Never, _ -> true
  | UnwindToken, UnwindToken -> true
  | UnwindToken, _ -> false
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
  | Fn expected, Fn actual -> type_check_fn expected actual variables
  | Fn _, _ -> false
  | Macro expected, Macro actual -> type_check_fn expected actual variables
  | Macro _, _ -> false
  | Dict { fields = expected_fields }, Dict { fields = actual_fields } ->
      StringMap.equal
        (fun a b -> type_check a b variables)
        expected_fields actual_fields
  | Dict _, _ -> false
  | Type, _ -> true
  | BuiltinMacro, BuiltinMacro -> true
  | BuiltinMacro, _ -> false
  | Template expected, Template actual -> true
  | Template _, _ -> false
  | NewType a, NewType b -> a = b (* todo should be typecheck? maybe not *)
  | NewType _, _ -> false
  | OneOf a, OneOf b ->
      StringMap.equal (Option.equal (fun a b -> type_check a b variables)) a b
  | OneOf _, _ -> false
  | Union a, Union b ->
      Seq.for_all
        (fun t -> Hashtbl.find_opt a t |> Option.is_some)
        (Hashtbl.to_seq_keys b)
  | Union a, t -> Hashtbl.find_opt a t |> Option.is_some
  | Var a, Var b -> a = b
  | Var id, actual -> (
      let rec unwrap_specifics = function
        | SpecificType t -> unwrap_specifics t
        | t -> t
      in
      let actual = unwrap_specifics actual in
      match Id.Map.find_opt id !variables with
      | None ->
          variables := Id.Map.add id actual !variables;
          true
      | Some value -> value = actual)
  | SpecificType a, SpecificType b -> a = b
  | SpecificType _, _ -> false

and type_of_fn : fn -> fn_type =
 fun f ->
  Log.trace "getting type of fun";
  let compiled = ensure_compiled f in
  let result_type = fully_infer_types compiled.body None in
  {
    result_type;
    arg_type = pattern_type_sure compiled.args None;
    contexts = compiled.contexts;
  }

and type_of_value : value -> value_type = function
  | Ast _ -> Ast
  | UnwindToken _ -> UnwindToken
  | Void -> Void
  | Variant { typ; _ } -> typ
  | BuiltinMacro _ -> BuiltinMacro
  | BuiltinFn { arg_type; result_type; contexts; _ } ->
      Fn { arg_type; result_type; contexts }
  | Template f ->
      Template
        {
          args_pattern = f.args_pattern;
          where_clause = f.where_clause;
          result_type = None;
          captured = f.captured;
          contexts = f.contexts;
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
  | Ref _ -> failwith "todo ref"
  | Struct { data; _ } ->
      Dict
        {
          fields =
            StringMap.map
              (fun (local : state_local) ->
                match local with
                | Binding _ -> failwith "binding wtf"
                | Value value -> type_of_value value)
              data.locals;
        }
  | Type t -> SpecificType t

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

and init_ir_data () : ir_data = { result_type = None; fully_inferred = NotYet }

and init_pattern_data () : ir_data =
  { result_type = None; fully_inferred = NotYet }

and just_value value expected_type =
  (match expected_type with
  | Some Any -> ()
  | Some expected_type ->
      let actual_type = type_of_value value in
      if not (type_check expected_type actual_type (empty_type_var_map ())) then
        failwith
          ("value " ^ show value ^ " is of wrong type (expected "
         ^ show_type expected_type ^ ", got " ^ show_type actual_type ^ ")")
  | None -> ());
  { value; new_bindings = StringMap.empty }

and update_locals =
  StringMap.union (fun _name _prev new_value -> Some new_value)

(* this is lexeme val from_string : (Cparser.token, 'a) MenhirLib.Convert.traditional -> string -> 'a *)

and pattern_match_opt (pattern : pattern) (value : value) :
    value StringMap.t option =
  Log.trace
    ("trying to pattern match " ^ show value ^ " with " ^ show_pattern pattern);
  match pattern with
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
  | Typed { pattern; typ; _ } ->
      if not (type_check typ (type_of_value value) (empty_type_var_map ())) then
        failwith "pattern specified different type"
      else pattern_match_opt pattern value
  | Dict { fields = field_patterns; _ } -> (
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

(* todo remove new_bindings args? *)
and compile_ast_to_ir (self : state) (ast : ast) ~(new_bindings : bool) :
    compiled =
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
  | Complex { def; values; _ } -> (
      match expand_macro self def.name values ~new_bindings with
      | Compiled result -> result
      | Pattern _ -> failwith "wtf")
  | Syntax { def; value; _ } -> compile_ast_to_ir self value ~new_bindings

and compile_pattern (self : state) (pattern : ast option) : pattern =
  match pattern with
  | None -> Void { data = init_pattern_data () }
  | Some ast -> (
      match ast with
      | Nothing _ -> Void { data = init_pattern_data () }
      | Simple { token; _ } -> (
          match token with
          | Ident ident ->
              Binding
                {
                  binding = { name = ident; value_type = None };
                  data = init_pattern_data ();
                }
          | Number raw -> failwith "todo pattern number"
          | String { value; raw } -> failwith "todo string pattern"
          | Punctuation _ -> failwith "punctuation")
      | Complex { def; values; _ } -> (
          match expand_macro self def.name values ~new_bindings:true with
          | Pattern result -> result
          | Compiled _ -> failwith @@ "wtf " ^ Ast.show ast)
      | Syntax { def; value; _ } ->
          failwith "syntax definition inlined in a pattern wtf?")

and forward_expected_type_to_pattern (pattern : pattern)
    (expected_type : value_type option) : unit =
  forward_expected_type_impl (pattern_data pattern) expected_type

and pattern_data (pattern : pattern) : pattern_data =
  match pattern with
  | Variant { data; _ }
  | Union { data; _ }
  | Void { data; _ }
  | Typed { data; _ }
  | Binding { data; _ }
  | Dict { data; _ } ->
      data

(*  todo not copypaste from maybe_infer_type *)
and maybe_infer_pattern_type (pattern : pattern)
    (expected_type : value_type option) ~(full : bool) : value_type option =
  forward_expected_type_to_pattern pattern expected_type;
  let data = pattern_data pattern in
  let result_type = (pattern_data pattern).result_type in
  let need_to_infer =
    Option.is_none data.result_type || (full && data.fully_inferred = NotYet)
  in
  if need_to_infer then (
    Log.trace "actually need to infer";
    if full then data.fully_inferred <- InProgress;
    let inferred : value_type option =
      match pattern with
      | Void _ -> Some Void
      | Variant { name; value; _ } ->
          (if full then
             match result_type with
             | Some result_type -> (
                 match result_type with
                 | OneOf variants -> (
                     let variant = StringMap.find name variants in
                     match (variant, value) with
                     | Some ty, Some pat ->
                         ignore (maybe_infer_pattern_type pat (Some ty) ~full)
                     | Some _, None -> failwith "pattern ex none, type has some"
                     | None, Some _ ->
                         failwith "pattern ex some, type has none "
                     | None, None -> ())
                 | _ -> failwith "variant on not oneof")
             | None -> ());
          result_type
      | Union { a; b; _ } -> (
          let a = maybe_infer_pattern_type a expected_type ~full in
          let b = maybe_infer_pattern_type b expected_type ~full in
          let to_set (t : value_type) =
            match t with
            | Union set -> set
            | _ ->
                let set = Hashtbl.create 1 in
                Hashtbl.add set t ();
                set
          in
          let merge a b =
            let new_set = Hashtbl.copy a in
            Hashtbl.add_seq new_set (Hashtbl.to_seq b);
            new_set
          in
          match (a, b) with
          | Some a, Some b -> Some (Union (merge (to_set a) (to_set b)))
          | _ -> None)
      | Typed { typ; pattern; _ } ->
          if full then
            ignore (maybe_infer_pattern_type pattern (Some typ) ~full);
          Some typ
      | Binding { binding; _ } -> (
          match (expected_type, binding.value_type) with
          | Some expected, None ->
              binding.value_type <- Some expected;
              Some expected
          | Some expected, Some specified ->
              if not (type_check expected specified (empty_type_var_map ()))
              then failwith "pattern type wrong";
              Some expected
          | None, _ -> binding.value_type)
      | Dict { fields; _ } -> (
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
                                    (maybe_infer_pattern_type field
                                       (Some expected_type) ~full))
                           | Some _field, None ->
                               failwith
                                 ("pattern specifies field " ^ name
                                ^ " which was not expected")
                           | None, Some _expected_type ->
                               failwith
                                 ("pattern does not specify field " ^ name)
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
                           match maybe_infer_pattern_type field None ~full with
                           | Some typ -> typ
                           | None -> failwith "todo partially inferred types")
                         fields;
                   }))
    in
    if full && Option.is_none inferred then
      failwith
        ("failed to infer type of pattern " ^ show_pattern pattern
       ^ " (expected "
        ^ show_or "<unknown>" show_type expected_type
        ^ ")");
    if full then data.fully_inferred <- Done;
    match inferred with
    | Some actual_type -> (
        match data.result_type with
        | None -> data.result_type <- Some actual_type
        | Some expected_type ->
            if
              not (type_check expected_type actual_type (empty_type_var_map ()))
            then
              failwith
                (show_pattern pattern ^ " expected " ^ show_type expected_type
               ^ ", got " ^ show_type actual_type))
    | None -> ());
  Log.trace
    ("inferred type of " ^ show_pattern pattern ^ " is "
    ^ show_or "<unknonwn>" show_type data.result_type);
  data.result_type

and pattern_type_sure (pattern : pattern) (expected_type : value_type option) :
    value_type =
  match maybe_infer_pattern_type pattern expected_type ~full:true with
  | Some typ -> typ
  | None ->
      failwith
        ("todo pattern type could not be figured out: " ^ show_pattern pattern)

and pattern_bindings (pattern : pattern) : binding StringMap.t =
  match pattern with
  | Void _ -> StringMap.empty
  | Typed { pattern; _ } -> pattern_bindings pattern
  | Variant { value; _ } -> (
      match value with
      | Some value -> pattern_bindings value
      | None -> StringMap.empty)
  | Binding { binding; _ } -> StringMap.singleton binding.name binding
  | Union { a; b; _ } ->
      let a = pattern_bindings a in
      let b = pattern_bindings b in
      if not (StringMap.equal (fun (a : binding) b -> a.name = b.name) a b) then
        failwith "pattern variants have different bindings";
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
  | false -> Compiled (compile_ast_to_ir self ast ~new_bindings)

and expand_macro (self : state) (name : string) (values : ast StringMap.t)
    ~(new_bindings : bool) : expanded_macro =
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
                    (fun arg -> (compile_ast_to_ir self arg ~new_bindings).ir)
                    values;
                data = init_ir_data ();
              }
          in
          Compiled
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
      | BuiltinMacro { impl; _ } -> impl self values ~new_bindings
      | Macro f -> (
          let values =
            Dict { fields = StringMap.map (fun x -> Ast x) values }
          in
          Log.trace ("call macro with " ^ show values);
          let compiled = ensure_compiled f in
          match call_compiled empty_contexts compiled values with
          | Ast new_ast ->
              Log.trace ("macro expanded to " ^ Ast.show new_ast);
              expand_ast self new_ast ~new_bindings
          | _ -> failwith "macro returned not an ast")
      | _ -> failwith (name ^ " is not a macro"))

and eval_ast (self : state) (ast : ast) (expected_type : value_type option) :
    evaled =
  let compiled = compile_ast_to_ir self ast ~new_bindings:false in
  Log.trace ("compiled: " ^ show_ir compiled.ir);
  eval_ir self compiled.ir expected_type

and forward_expected_type_impl (data : type_inference_data)
    (expected_type : value_type option) : unit =
  match expected_type with
  | Some expected_type -> (
      match data.result_type with
      | Some actual_type ->
          if not (type_check expected_type actual_type (empty_type_var_map ()))
          then
            failwith
              ("expected " ^ show_type expected_type ^ ", got "
             ^ show_type actual_type)
      | None -> data.result_type <- Some expected_type)
  | None -> ()

and forward_expected_type (ir : ir) (expected_type : value_type option) : unit =
  forward_expected_type_impl (ir_data ir) expected_type

and maybe_infer_types (ir : ir) (expected_type : value_type option)
    ~(full : bool) : value_type option =
  let ir_data = ir_data ir in
  Log.trace
    ("maybe inferring type of " ^ show_ir ir ^ " expecting "
    ^ show_or "<unknown>" show_type expected_type);
  forward_expected_type ir expected_type;
  let need_to_infer =
    Option.is_none ir_data.result_type
    || (full && ir_data.fully_inferred = NotYet)
  in
  if need_to_infer then (
    Log.trace "actually need to infer";
    if full then ir_data.fully_inferred <- InProgress;
    let inferred : value_type option =
      match ir with
      | Void _ -> Some Void
      | Struct { body; _ } ->
          (* todo *)
          Some Any
      | NewType { def; _ } ->
          if full then ignore @@ pattern_type_sure def None;
          Some Type
      | CreateImpl { captured; ty; trait; impl; _ } ->
          if full then (
            let ty = value_to_type (eval_ir captured ty (Some Type)).value in
            ignore @@ fully_infer_types trait (Some (Type : value_type));
            ignore @@ fully_infer_types impl (Some ty));
          Some Void
      | GetImpl { captured; ty; trait; _ } ->
          if full then
            ignore @@ value_to_type (eval_ir captured ty (Some Type)).value;
          let trait =
            value_to_type (eval_ir captured trait (Some Type)).value
          in
          Some trait
      | CheckImpl { captured; ty; trait; _ } ->
          if full then (
            ignore @@ value_to_type (eval_ir captured ty (Some Type)).value;
            ignore @@ value_to_type (eval_ir captured trait (Some Type)).value);
          Some Bool
      | OneOf { variants; _ } ->
          if full then
            StringMap.iter
              (fun _name variant ->
                match variant with
                | Some variant ->
                    ignore
                    @@ fully_infer_types variant (Some (Type : value_type))
                | None -> ())
              variants;
          Some Type
      | Scope { expr; _ } -> maybe_infer_types expr expected_type ~full
      | Unwinding { f; _ } -> (
          match maybe_infer_types f None ~full with
          | Some (Fn f) ->
              let { result_type; _ } = f in
              Some result_type
          | Some Any -> None
          | Some typ ->
              failwith
                (show_ir f ^ " is type " ^ show_type typ ^ " which is not fun")
          | None -> None)
      | WithContext { new_context; expr; _ } ->
          if full then ignore (fully_infer_types new_context None);
          maybe_infer_types expr expected_type ~full
      | CurrentContext { context_type; _ } -> Some context_type
      | TypeOf { captured; expr; _ } -> Some Type
      | TypeOfValue { captured; expr; _ } -> Some Type
      | BuiltinFn { f = { arg_type; result_type; contexts; _ }; _ } ->
          Some (Fn { arg_type; result_type; contexts })
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
                  arg_type =
                    Option.get
                      (maybe_infer_pattern_type compiled.args None ~full:true);
                  result_type =
                    Option.get (maybe_infer_types compiled.body None ~full);
                  contexts = compiled.contexts;
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
                       let inferred =
                         match
                           maybe_infer_types value
                             (StringMap.find_opt name field_types)
                             ~full:true
                         with
                         | Some field_type -> field_type
                         | None ->
                             failwith
                               ("failed to infer type of field " ^ name
                              ^ " with value " ^ show_ir value)
                       in
                       Log.trace
                         ("field inferred type " ^ name ^ " (value = "
                        ^ show_ir value ^ ") is " ^ show_type inferred);
                       inferred)
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
          | Type | Any -> Some Any
          | _ -> failwith "todo field access")
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
                     contexts = compiled.contexts;
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
                  eval_ir captured args
                    (maybe_infer_pattern_type compiled.args None ~full)
                in
                Log.trace
                  ("instantiating type inf " ^ show_pattern compiled.args
                 ^ " => " ^ show_ir compiled.body);
                Some
                  (value_to_type
                     (call_compiled empty_contexts compiled args.value))
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
          | Some BuiltinMacro -> expected_type
          | Some Any -> Some Any
          | Some typ ->
              failwith
                (show_ir f ^ " is type " ^ show_type typ ^ " which is not fun")
          | None -> None)
      | Match { value; branches; _ } ->
          let value_type = maybe_infer_types value None ~full:false in
          let value_type =
            List.fold_right
              (fun branch cur ->
                maybe_infer_pattern_type branch.pattern cur ~full:false)
              branches value_type
          in
          let result =
            List.fold_right
              (fun branch cur -> maybe_infer_types branch.body cur ~full:false)
              branches expected_type
          in
          if full then (
            let value_type =
              Option.get
                (List.fold_right
                   (fun branch cur ->
                     Some (pattern_type_sure branch.pattern cur))
                   branches value_type)
            in
            ignore (fully_infer_types value (Some value_type));
            List.fold_right
              (fun branch cur -> maybe_infer_types branch.body cur ~full:false)
              branches result)
          else result
      | If { cond; then_case; else_case; _ } ->
          if full then ignore (fully_infer_types cond (Some Bool));
          let then_type = maybe_infer_types then_case None ~full:false in
          let else_type = maybe_infer_types else_case then_type ~full in
          let common_type =
            match (then_type, else_type) with
            | Some then_case, Some else_case ->
                (* todo same_types instead of type_check? *)
                if not (type_check then_case else_case (empty_type_var_map ()))
                then
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
             let pattern_type =
               maybe_infer_pattern_type pattern None ~full:false
             in
             let value_type =
               maybe_infer_types value pattern_type ~full:false
             in
             let pattern_type =
               maybe_infer_pattern_type pattern value_type ~full:false
             in
             let value_type = fully_infer_types value pattern_type in
             let pattern_type = pattern_type_sure pattern (Some value_type) in
             ignore ((pattern_type, value_type) : value_type * value_type));
          Some Void
    in
    if full && Option.is_none inferred then
      failwith
        ("failed to infer type of " ^ show_ir ir ^ " (expected "
        ^ show_or "<unknown>" show_type expected_type
        ^ ")");
    if full then ir_data.fully_inferred <- Done;
    match inferred with
    | Some actual_type -> (
        match ir_data.result_type with
        | None -> ir_data.result_type <- Some actual_type
        | Some expected_type ->
            if
              not (type_check expected_type actual_type (empty_type_var_map ()))
            then
              failwith
                (show_ir ir ^ " expected " ^ show_type expected_type ^ ", got "
               ^ show_type actual_type))
    | None -> ());
  Log.trace
    ("inferred type of " ^ show_ir ir ^ " is "
    ^ show_or "<unknonwn>" show_type ir_data.result_type);
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
    | Struct { body; _ } ->
        Log.info "evaluating struct";
        let evaled = eval_ir self body (Some Void) in
        just_value
          (Struct
             {
               parent = Some self.self;
               data =
                 {
                   syntax = Syntax.empty;
                   locals =
                     StringMap.map
                       (fun value : state_local -> Value value)
                       evaled.new_bindings;
                 };
             })
          result_type
    | NewType { def; _ } ->
        just_value (Type (NewType (pattern_type_sure def None))) result_type
    | Scope { expr; _ } ->
        just_value (eval_ir self expr expected_type).value result_type
    | CreateImpl { trait; ty; impl; _ } ->
        let trait = value_to_type (eval_ir self trait (Some Type)).value in
        let ty = value_to_type (eval_ir self ty (Some Type)).value in
        let impl_def : impl_def = { ty; trait } in
        let impl = (eval_ir self impl (Some trait)).value in
        Hashtbl.add trait_impls impl_def impl;
        just_value Void result_type
    | GetImpl { trait; ty; _ } ->
        let trait = value_to_type (eval_ir self trait (Some Type)).value in
        let ty = value_to_type (eval_ir self ty (Some Type)).value in
        let impl_def : impl_def = { ty; trait } in
        let impl =
          match Hashtbl.find_opt trait_impls impl_def with
          | Some impl -> impl
          | None ->
              failwith @@ "get_impl failed: " ^ show_type trait
              ^ " is not implemented for " ^ show_type ty
        in
        just_value impl result_type
    | CheckImpl { trait; ty; _ } ->
        let trait = value_to_type (eval_ir self trait (Some Type)).value in
        let ty = value_to_type (eval_ir self ty (Some Type)).value in
        let impl_def : impl_def = { ty; trait } in
        let result = Hashtbl.find_opt trait_impls impl_def |> Option.is_some in
        just_value (Bool result) result_type
    | Match { value; branches; _ } -> (
        let value = (eval_ir self value None).value in
        let result : value option =
          List.find_map
            (fun branch ->
              pattern_match_opt branch.pattern value
              |> Option.map (fun new_bindings ->
                     let self_with_new_bindings =
                       {
                         self with
                         data =
                           {
                             self.data with
                             locals =
                               update_locals self.data.locals
                               @@ StringMap.map
                                    (fun value -> Value value)
                                    new_bindings;
                           };
                       }
                     in
                     (eval_ir self_with_new_bindings branch.body result_type)
                       .value))
            branches
        in
        match result with
        | Some value -> just_value value result_type
        | None -> failwith "match failed")
    | OneOf { variants; _ } ->
        just_value
          (Type
             (OneOf
                (StringMap.map
                   (Option.map (fun variant ->
                        value_to_type
                        @@ (eval_ir self variant (Some Type)).value))
                   variants)))
          result_type
    | TypeOf { captured; expr; _ } ->
        just_value (Type (fully_infer_types expr (Some Type))) (Some Type)
    | TypeOfValue { captured; expr; _ } ->
        just_value
          (Type (type_of_value (eval_ir self expr (Some Any)).value))
          (Some Type)
    | BuiltinFn { f; _ } -> just_value (BuiltinFn f) result_type
    | Unwinding { f; _ } ->
        just_value
          (match (eval_ir self f None).value with
          | Function f -> (
              let compiled = ensure_compiled f in
              let token = Id.gen () in
              try call_compiled self.contexts compiled (UnwindToken token)
              with Unwind (unwinded_token, value) ->
                if unwinded_token = token then value
                else raise (Unwind (unwinded_token, value)))
          | _ -> failwith "unwinding must take a function")
          result_type
    | WithContext { new_context; expr; _ } ->
        let new_context = (eval_ir self new_context None).value in
        let new_state =
          {
            self with
            contexts =
              (let new_contexts = Hashtbl.copy self.contexts in
               let context_type = type_of_value new_context in
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
        just_value (eval_ir new_state expr expected_type).value result_type
    | CurrentContext { context_type; _ } -> (
        let all_current =
          match Hashtbl.find_opt self.contexts context_type with
          | Some current -> current
          | None -> []
        in
        match head all_current with
        | Some top -> just_value top result_type
        | None ->
            failwith
              ("context not available: " ^ show_type context_type
             ^ ", current contexts: "
              ^ show_contexts self.contexts))
    | Dict { fields; _ } ->
        let field_types =
          match result_type with
          | Some (Dict { fields }) -> fields
          | Some Type | Some Any | None -> StringMap.empty (* todo None? *)
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
    | FieldAccess { obj; name; default_value; _ } ->
        let obj = (eval_ir self obj None).value in
        let value =
          match get_field_opt obj name with
          | Some value -> value
          | None -> (
              match default_value with
              | Some default -> (eval_ir self default expected_type).value
              | None ->
                  failwith ("field " ^ name ^ " does not exist in " ^ show obj))
        in
        just_value value result_type
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
              ( call_compiled empty_contexts compiled,
                maybe_infer_pattern_type compiled.args
                  (ir_data args).result_type ~full:false )
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
              ( call_compiled self.contexts compiled,
                maybe_infer_pattern_type compiled.args
                  (ir_data args).result_type ~full:false )
          | Macro f ->
              let compiled = ensure_compiled f in
              ( call_compiled empty_contexts compiled,
                maybe_infer_pattern_type compiled.args
                  (ir_data args).result_type ~full:false )
          | BuiltinMacro { impl; _ } ->
              ( (function
                | Dict { fields } ->
                    let ir =
                      match
                        impl self
                          (StringMap.map
                             (function
                               | Ast ast -> ast
                               | _ ->
                                   failwith
                                     "builtin macro arg must be dict of asts")
                             fields)
                          ~new_bindings:false
                      with
                      | Compiled { ir; _ } -> ir
                      | Pattern _ -> failwith "wtf"
                    in
                    (eval_ir self ir expected_type).value
                | _ -> failwith "builtin macro arg must be dict of asts"),
                None (* todo *) )
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
    | If { cond; then_case; else_case; _ } ->
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
        just_value
          (match cond.value with
          | Bool true -> eval_ir self_with_new_bindings then_case common_type
          | Bool false -> eval_ir self_with_new_bindings else_case common_type
          | _ -> failwith "condition must be a bool")
            .value result_type
    | Let { pattern; value; _ } ->
        Log.trace @@ "let " ^ show_pattern pattern ^ " = " ^ show_ir value;
        let result =
          {
            value = Void;
            new_bindings =
              pattern_match pattern
                (eval_ir self value
                   (maybe_infer_pattern_type pattern (ir_data value).result_type
                      ~full:false))
                  .value;
          }
        in
        Log.trace @@ "let value is " ^ show result.value;
        result
  in
  Log.trace
    ("evaluated " ^ show_ir ir ^ " = " ^ show result.value ^ " as "
    ^ show_or "<unknown>" show_type result_type);
  match result_type with
  | Some Type -> { result with value = Type (value_to_type result.value) }
  | _ -> result

and value_to_type : value -> value_type = function
  | Void -> Void
  | Type t -> t
  | Dict { fields } -> Dict { fields = StringMap.map value_to_type fields }
  | Struct { data; _ } ->
      Dict
        {
          fields =
            StringMap.map
              (fun (local : state_local) ->
                match local with
                | Binding _ -> failwith "binding wtf"
                | Value value -> value_to_type value)
              data.locals;
        }
  | Template f -> Template f
  | other -> failwith (show other ^ " is not a type")

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
           where_clause =
             (match f.where_clause with
             | None -> Const { value = Bool true; data = init_ir_data () }
             | Some clause ->
                 (compile_ast_to_ir captured clause ~new_bindings:false).ir);
           args;
           result_type =
             (match f.result_type with
             | None -> None
             | Some (Actual t) ->
                 Some (Const { value = Type t; data = init_ir_data () })
             | Some (Ast ast) ->
                 Some (compile_ast_to_ir captured ast ~new_bindings:false).ir);
           body = (compile_ast_to_ir captured f.body ~new_bindings:false).ir;
           contexts =
             (* todo expecte type contexts_type *)
             (match f.contexts with
             | Some contexts ->
                 value_to_contexts_type
                   (eval_ast f.captured contexts None).value
             | None -> default_contexts_type);
         }));
  Option.get f.compiled

and value_to_contexts_type (value : value) : contexts_type =
  let result = Hashtbl.create 1 in
  Hashtbl.add result (value_to_type value) 1;
  result

and call_compiled (current_contexts : contexts) (f : compiled_fn) (args : value)
    : value =
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
  (match (eval_ir captured f.where_clause (Some Bool)).value with
  | Bool true -> ()
  | Bool false ->
      failwith @@ "where clause failed: " ^ show_ir f.where_clause ^ ", args = "
      ^ show args
  | value ->
      failwith @@ "where clause evaluated to " ^ show value
      ^ ", expected a bool");
  (* todo lazy *)
  let expected_type =
    Option.map
      (fun ir -> value_to_type (eval_ir f.captured ir (Some Type)).value)
      f.result_type
  in
  Log.trace
    ("calling " ^ show_ir f.body ^ " with " ^ show args ^ " expecting "
    ^ show_or "<unknown>" show_type expected_type);
  (eval_ir { captured with contexts = current_contexts } f.body expected_type)
    .value

and discard : value -> unit = function
  | Void -> ()
  | that -> failwith ("only void can be discarded (discarded " ^ show that ^ ")")

and get_field_opt (obj : value) (field : string) : value option =
  match obj with
  | Dict { fields = dict } -> StringMap.find_opt field dict
  | Type (OneOf variants as typ) ->
      Option.map
        (fun variant ->
          match variant with
          | Some variant ->
              BuiltinFn
                {
                  name = "type constructor";
                  arg_type = variant;
                  result_type = typ;
                  contexts = empty_contexts_type;
                  impl =
                    (fun value ->
                      Variant { name = field; typ; value = Some value });
                }
          | None -> Variant { name = field; typ; value = None })
        (StringMap.find_opt field variants)
  | _ -> failwith "can't get field of this thing"

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
            (eval_ast self (StringMap.find "type" args) (Some Type)).value
          in
          match typ with
          | Type typ -> (
              match value with
              | Compiled compiled ->
                  forward_expected_type compiled.ir (Some typ);
                  Compiled compiled
              | Pattern (Binding { binding; data }) ->
                  binding.value_type <- Some typ;
                  Pattern (Binding { binding; data })
              | Pattern pattern ->
                  Pattern (Typed { pattern; typ; data = init_pattern_data () }))
          | _ -> failwith "type is not a type");
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
                (Variant
                   { name; value = Some value; data = init_pattern_data () })
          | false ->
              let f = compile_ast_to_ir self f ~new_bindings in
              let args = compile_ast_to_ir self args ~new_bindings in
              Compiled
                {
                  ir = Call { f = f.ir; args = args.ir; data = init_ir_data () };
                  new_bindings = StringMap.empty;
                });
    }

  let instantiate_template : builtin_macro =
    {
      name = "instantiate_template";
      impl =
        (fun self args ~new_bindings ->
          let template =
            compile_ast_to_ir self
              (StringMap.find "template" args)
              ~new_bindings
          in
          let args =
            compile_ast_to_ir self (StringMap.find "args" args) ~new_bindings
          in
          Compiled
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
            });
    }

  let then' : builtin_macro =
    {
      name = "then";
      impl =
        (fun self args ~new_bindings ->
          let a =
            compile_ast_to_ir self (StringMap.find "a" args) ~new_bindings
          in
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
            | Some b -> compile_ast_to_ir self_with_new_bindings b ~new_bindings
            | None ->
                {
                  ir = Void { data = init_ir_data () };
                  new_bindings = StringMap.empty;
                }
          in
          Compiled
            {
              ir = Then { first = a.ir; second = b.ir; data = init_ir_data () };
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
      contexts = io_contexts;
      arg_type = String;
      result_type = Void;
    }

  let match' : builtin_macro =
    {
      name = "match";
      impl =
        (fun self args ~new_bindings ->
          let value =
            compile_ast_to_ir self (StringMap.find "value" args) ~new_bindings
          in
          let rec collect_branches (ast : ast) : ir_data match_branch list =
            match ast with
            | Complex { def = { name = "combine_variants"; _ }; values; _ } -> (
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
                              |> StringMap.map (fun binding : state_local ->
                                     Binding binding));
                        };
                    }
                    body ~new_bindings:false
                in
                [ { pattern; body = body.ir } ]
            | _ -> failwith "match syntax wrong"
          in
          let branches = collect_branches (StringMap.find "branches" args) in
          Compiled
            {
              ir = Match { value = value.ir; branches; data = init_ir_data () };
              new_bindings = StringMap.empty;
            });
    }

  let if' : builtin_macro =
    {
      name = "if";
      impl =
        (fun self args ~new_bindings ->
          let cond =
            compile_ast_to_ir self (StringMap.find "cond" args) ~new_bindings
          in
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
               (StringMap.find "then" args)
               ~new_bindings)
              .ir
          in
          let else' =
            match StringMap.find_opt "else" args with
            | Some branch ->
                (compile_ast_to_ir self_with_new_bindings branch ~new_bindings)
                  .ir
            | None -> Void { data = init_ir_data () }
          in
          Compiled
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
            });
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
      (* todo overflows? *)
      contexts = default_contexts_type;
      result_type = Int32;
    }

  let int32_binary_op name = int32_binary_op_with name "lhs" "rhs"

  let float64_fn name f : builtin_fn =
    {
      name;
      impl =
        (function
        | Float64 value -> Float64 (f value) | _ -> failwith "only floats");
      arg_type = Float64;
      result_type = Float64;
      (* todo? *)
      contexts = default_contexts_type;
    }

  let int32_fn name f : builtin_fn =
    {
      name;
      impl =
        (function
        | Int32 value -> Int32 (f value) | _ -> failwith "only floats");
      arg_type = Int32;
      result_type = Int32;
      (* todo ? *)
      contexts = default_contexts_type;
    }

  let single_arg_fn fn_name arg_type arg_name result_type f : builtin_fn =
    {
      name = fn_name;
      impl =
        dict_fn (fun args ->
            let value = StringMap.find arg_name args in
            f value);
      arg_type = Dict { fields = StringMap.singleton arg_name arg_type };
      (* todo ? *)
      contexts = default_contexts_type;
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
                      {
                        expr = (compile_ast_to_ir self e ~new_bindings).ir;
                        data = init_ir_data ();
                      };
                  new_bindings = StringMap.empty;
                });
    }

  let with_context : builtin_macro =
    {
      name = "with_context";
      impl =
        (fun self args ~new_bindings ->
          let new_context =
            compile_ast_to_ir self
              (StringMap.find "new_context" args)
              ~new_bindings
          in
          let expr =
            compile_ast_to_ir self (StringMap.find "expr" args) ~new_bindings
          in
          Compiled
            {
              ir =
                WithContext
                  {
                    expr = expr.ir;
                    new_context = new_context.ir;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let comptime : builtin_macro =
    {
      name = "comptime";
      impl =
        (fun self args ~new_bindings ->
          let value = StringMap.find "value" args in
          Compiled
            {
              ir =
                Const
                  {
                    value = (eval_ast self value None).value;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let quote : builtin_macro =
    {
      name = "quote";
      impl =
        (fun self args ~new_bindings ->
          let rec impl : ast -> ir = function
            | Complex { def = { name = "unquote"; _ }; values; _ } ->
                let inner = StringMap.find "expr" values in
                Log.trace ("unquoting" ^ Ast.show inner);
                (compile_ast_to_ir self inner ~new_bindings).ir
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
          let value = (compile_ast_to_ir self value ~new_bindings).ir in
          Compiled
            {
              ir = Let { pattern; value; data = init_ir_data () };
              new_bindings =
                StringMap.map
                  (fun binding : state_local -> Binding binding)
                  (pattern_bindings pattern);
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
            | Pattern _ -> failwith "wtf"
          in
          match ir with
          | Let { pattern; value; _ } ->
              let value_type =
                maybe_infer_pattern_type pattern None ~full:false
              in
              let evaled = eval_ir self value value_type in
              let let_ir =
                Let
                  {
                    pattern;
                    value =
                      Const { value = evaled.value; data = init_ir_data () };
                    data = init_ir_data ();
                  }
              in
              let evaled = eval_ir self let_ir (Some Void) in
              Compiled
                {
                  ir = let_ir;
                  new_bindings =
                    StringMap.map (fun value -> Value value) evaled.new_bindings;
                }
          | _ -> failwith "let compiled into not a let wtf?");
    }

  let template_def : builtin_macro =
    {
      name = "template_def";
      impl =
        (fun self args ~new_bindings ->
          let where = StringMap.find_opt "where" args in
          let def = StringMap.find "def" args in
          let args = StringMap.find "args" args in
          Compiled
            {
              ir =
                Template
                  {
                    f =
                      {
                        captured = self;
                        where_clause = where;
                        result_type = Some (Actual Any);
                        args_pattern = Some args;
                        body = def;
                        contexts = None;
                        compiled = None;
                      };
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let function_def : builtin_macro =
    {
      name = "function_def";
      impl =
        (fun self args ~new_bindings ->
          let args_pattern = StringMap.find_opt "args" args in
          let where = StringMap.find_opt "where" args in
          let result_type = StringMap.find_opt "result_type" args in
          let contexts = StringMap.find_opt "contexts" args in
          let body = StringMap.find "body" args in
          Compiled
            {
              ir =
                Function
                  {
                    f =
                      {
                        where_clause = where;
                        result_type =
                          Option.map
                            (fun t : fn_result_type -> Ast t)
                            result_type;
                        captured = self;
                        args_pattern;
                        contexts;
                        body;
                        compiled = None;
                      };
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let field_access : builtin_macro =
    {
      name = "field_access";
      impl =
        (fun self args ~new_bindings ->
          let obj = StringMap.find "obj" args in
          let obj = (compile_ast_to_ir self obj ~new_bindings).ir in
          let field = StringMap.find "field" args in
          let default_value = StringMap.find_opt "default_value" args in
          let default_value =
            Option.map
              (fun ast ->
                Log.trace ("default = " ^ Ast.show ast);
                (compile_ast_to_ir self ast ~new_bindings).ir)
              default_value
          in
          match field with
          | Simple { token = Ident name; _ } ->
              Compiled
                {
                  ir =
                    FieldAccess
                      { obj; name; default_value; data = init_ir_data () };
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
                     data = init_pattern_data ();
                   })
          | false ->
              Compiled
                {
                  ir =
                    Dict
                      {
                        fields =
                          StringMap.singleton name
                            (compile_ast_to_ir self value ~new_bindings).ir;
                        data = init_ir_data ();
                      };
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
                      value_to_type
                        (eval_ast self context_type (Some Type)).value;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let typeof : builtin_macro =
    {
      name = "typeof";
      impl =
        (fun self args ~new_bindings ->
          let expr = StringMap.find "expr" args in
          Compiled
            {
              ir =
                TypeOf
                  {
                    captured = self;
                    expr = (compile_ast_to_ir self expr ~new_bindings).ir;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  (*  todo fn instead of macro *)
  let typeofvalue : builtin_macro =
    {
      name = "typeofvalue";
      impl =
        (fun self args ~new_bindings ->
          let expr = StringMap.find "expr" args in
          Compiled
            {
              ir =
                TypeOfValue
                  {
                    captured = self;
                    expr = (compile_ast_to_ir self expr ~new_bindings).ir;
                    data = init_ir_data ();
                  };
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
            | Pattern _ -> failwith "expected a dict"
            | Compiled _ -> failwith "wtf"
          in
          let fields : expanded_macro -> _ = function
            | Compiled { ir = Dict { fields; _ }; _ } -> fields
            | Compiled _ -> failwith "expected a dict"
            | Pattern _ -> failwith "wtf"
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
                         data = init_pattern_data ();
                       })
              | false ->
                  Compiled
                    {
                      ir =
                        Dict
                          {
                            fields =
                              StringMap.union
                                (fun name _a _b ->
                                  failwith
                                    (name ^ " is specified multiple times"))
                                (fields a) (fields b);
                            data = init_ir_data ();
                          };
                      new_bindings = StringMap.empty;
                    })
          | None -> (
              match new_bindings with
              | true ->
                  Pattern
                    (Dict
                       {
                         fields = pattern_fields a;
                         data = init_pattern_data ();
                       })
              | false ->
                  Compiled
                    {
                      ir = Dict { fields = fields a; data = init_ir_data () };
                      new_bindings = StringMap.empty;
                    }));
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
      (* todo ? *)
      contexts = default_contexts_type;
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
      (* todo ? *)
      contexts = default_contexts_type;
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
            let result = Type (Fn { arg_type; result_type; contexts }) in
            Log.trace (show result);
            result
        | _ -> failwith "expected dict");
      arg_type = Any;
      (* todo *)
      (* Dict *)
      (* { *)
      (* fields = *)
      (* StringMap.of_list *)
      (* [ ("arg", (Type : value_type)); ("result", Type) ]; *)
      (* }; *)
      contexts = empty_contexts_type;
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
      (* todo rng *)
      contexts = default_contexts_type;
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
      (* todo rng *)
      contexts = default_contexts_type;
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
      (* todo ? *)
      contexts = default_contexts_type;
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
      contexts = empty_contexts_type;
      result_type = Bool;
    }

  let unwinding : builtin_macro =
    {
      name = "unwinding";
      impl =
        (fun self args ~new_bindings ->
          let def = StringMap.find "def" args in
          Compiled
            {
              ir =
                Unwinding
                  {
                    f = (compile_ast_to_ir self def ~new_bindings).ir;
                    data = init_ir_data ();
                  };
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
      arg_type =
        Dict
          {
            fields =
              StringMap.of_list
                [ ("token", (UnwindToken : value_type)); ("value", Any) ];
          };
      contexts = empty_contexts_type;
      result_type = Never;
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
          | true -> Pattern (Union { a; b; data = init_pattern_data () })
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
            Pattern (Variant { name; value; data = init_pattern_data () })
          else
            let typ =
              Option.map
                (fun typ -> (compile_ast_to_ir self typ ~new_bindings).ir)
                (StringMap.find_opt "type" args)
            in
            Compiled
              {
                ir =
                  OneOf
                    {
                      variants = StringMap.singleton name typ;
                      data = init_ir_data ();
                    };
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
                  (compile_ast_to_ir self (StringMap.find name args)
                     ~new_bindings)
                    .ir
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
                  ir = OneOf { variants; data = init_ir_data () };
                  new_bindings = StringMap.empty;
                }
          | true -> failwith "combine variants is only a expr");
    }

  let create_impl : builtin_macro =
    {
      name = "create_impl";
      impl =
        (fun self args ~new_bindings ->
          let ty = StringMap.find "type" args in
          let trait = StringMap.find "trait" args in
          let impl = StringMap.find "impl" args in
          Compiled
            {
              ir =
                CreateImpl
                  {
                    captured = self;
                    ty = (compile_ast_to_ir self ty ~new_bindings).ir;
                    trait = (compile_ast_to_ir self trait ~new_bindings).ir;
                    impl = (compile_ast_to_ir self impl ~new_bindings).ir;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let get_impl : builtin_macro =
    {
      name = "get_impl";
      impl =
        (fun self args ~new_bindings ->
          let ty = StringMap.find "type" args in
          let trait = StringMap.find "trait" args in
          Compiled
            {
              ir =
                GetImpl
                  {
                    captured = self;
                    ty = (compile_ast_to_ir self ty ~new_bindings).ir;
                    trait = (compile_ast_to_ir self trait ~new_bindings).ir;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let check_impl : builtin_macro =
    {
      name = "check_impl";
      impl =
        (fun self args ~new_bindings ->
          let ty = StringMap.find "type" args in
          let trait = StringMap.find "trait" args in
          Compiled
            {
              ir =
                CheckImpl
                  {
                    captured = self;
                    ty = (compile_ast_to_ir self ty ~new_bindings).ir;
                    trait = (compile_ast_to_ir self trait ~new_bindings).ir;
                    data = init_ir_data ();
                  };
              new_bindings = StringMap.empty;
            });
    }

  let struct_def : builtin_macro =
    {
      name = "struct_def";
      impl =
        (fun self args ~new_bindings ->
          let body = StringMap.find "body" args in
          let body = (compile_ast_to_ir self body ~new_bindings).ir in
          Compiled
            {
              ir = Struct { body; data = init_ir_data () };
              new_bindings = StringMap.empty;
            });
    }

  let new_typevar : builtin_fn =
    {
      name = "new_typevar";
      contexts = empty_contexts_type;
      arg_type = Void;
      result_type = Type;
      impl =
        (fun _void ->
          Log.info "making new type var";
          Type (Var (Id.gen ())));
    }

  let all =
    StringMap.of_list
      [
        ("typevar", BuiltinFn new_typevar);
        ("struct_def", BuiltinMacro struct_def);
        ("create_impl", BuiltinMacro create_impl);
        ("get_impl", BuiltinMacro get_impl);
        ("check_impl", BuiltinMacro check_impl);
        ("single_variant", BuiltinMacro single_variant);
        ("combine_variants", BuiltinMacro combine_variants);
        ("unwind", BuiltinFn unwind);
        ("unwind_token", Type UnwindToken);
        ("unwinding", BuiltinMacro unwinding);
        ("current_context", BuiltinMacro current_context);
        ("typeof", BuiltinMacro typeof);
        ("typeofvalue", BuiltinMacro typeofvalue);
        ("any", Type Any);
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
        ("type_ascribe", BuiltinMacro type_ascribe);
        ("print", BuiltinFn print);
        ("dbg", BuiltinFn dbg);
        ("call", BuiltinMacro call);
        ("then", BuiltinMacro then');
        ("if", BuiltinMacro if');
        ("match", BuiltinMacro match');
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
              contexts = empty_contexts_type;
              result_type = Type;
            } );
        ( "unit",
          BuiltinFn
            {
              name = "unit(void)";
              impl = (fun _ -> Void);
              arg_type = Dict { fields = StringMap.empty };
              contexts = empty_contexts_type;
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
              (* todo *)
              contexts = default_contexts_type;
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
              contexts = io_contexts;
              result_type = String;
            } );
        ("template_def", BuiltinMacro template_def);
        ("instantiate_template", BuiltinMacro instantiate_template);
        ("is_same_type", BuiltinFn is_same_type);
        ("panic", BuiltinFn panic);
        ("comptime", BuiltinMacro comptime);
        ("never", Type Never);
        ("with_context", BuiltinMacro with_context);
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
