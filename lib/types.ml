open Prelude

type ast_data = { span : Span.span }
type ast = ast_data Ast.node
type id = Id.t

type value =
  | Binding of binding
  | Var of { id : id; typ : value_type }
  | InferVar of inference_var
  | UnwindToken of id
  | DelimitedToken of id
  | Ast of ast
  | Ir of ir
  | Macro of fn
  | BuiltinMacro of builtin_macro
  | BuiltinFn of { f : builtin_fn; ty : fn_type option }
  | BuiltinTemplate of { f : builtin_fn; ty : value_type }
  | Template of fn
  | Function of fn
  | Void
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Float64 of float
  | String of string
  | Tuple of { unnamed_fields : value list; named_fields : value StringMap.t }
  | List of { values : value list; ty : value_type }
  | Struct of struct'
  | Ref of value ref
  | Type of value_type
  | Variant of { typ : value_type; name : string; value : value option }
  | MultiSet of value list
  | Builtin of { name : string; ty : value_type }

and value_type =
  | Var of { id : id }
  | Binding of binding
  | UnwindToken
  | DelimitedToken
  | Never
  | Ast
  | Ir
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
  | Builtin of string
  | BuiltinMacro
  | Tuple of {
      unnamed_fields : value_type list;
      named_fields : value_type StringMap.t;
    }
  | List of value_type
  | NewType of value_type
  | OneOf of value_type option StringMap.t
  | Union of Id.Set.t
  | Type
  | InferVar of inference_var
  | MultiSet of value_type
  | MultiSetOldToRemove of int Id.Map.t

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

and builtin_fn = { name : string; impl : fn_type -> value -> value }

and builtin_macro = {
  name : string;
  impl : state -> ast StringMap.t -> new_bindings:bool -> expanded_macro;
}

and 'data match_branch = { pattern : pattern; body : ir }
and 'data get_impl = { captured : state; value : ir; trait : ir; data : 'data }

and 'data ir_node =
  | Void of { data : 'data }
  | NewType of { def : ir; data : 'data }
  | Use of { namespace : value; data : 'data }
  | Struct of { body : ir; field_types : value_type StringMap.t; data : 'data }
  | Assign of { pattern : pattern; value : ir; data : 'data }
  | CreateImpl of {
      captured : state;
      value : ir;
      trait : ir;
      impl : ir;
      data : 'data;
    }
  | GetImpl of 'data get_impl
  | CheckImpl of 'data get_impl
  | Match of { value : ir; branches : ir_data match_branch list; data : 'data }
  | Scope of { expr : ir; data : 'data }
  | ConstructVariant of {
      ty : value_type option;
      variant : string;
      value : ir option;
      data : 'data;
    }
  | OneOf of { variants : ir option StringMap.t; data : 'data }
  | TypeOf of { captured : state; expr : ir; data : 'data }
  | TypeOfValue of { captured : state; expr : ir; data : 'data }
  | Tuple of {
      unnamed_fields : ir list;
      named_fields : ir StringMap.t;
      data : 'data;
    }
  | UnwindableBlock of { f : ir; data : 'data }
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
  | Builtin of { name : string; data : 'data } (* TODO remove BuiltinFn *)
  | BuiltinFn of { f : builtin_fn; data : 'data }
  | If of { cond : ir; then_case : ir; else_case : ir; data : 'data }
  | Let of { pattern : pattern; value : ir; data : 'data }
  | MultiSet of { a : ir; b : ir option; data : 'data }

and inference_status = NotYet | InProgress | Done
and type_inference_data = { type_var : inference_var }
and no_data = NoData
and ir_data = { inference : type_inference_data; state : state }
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
  shared_compiled_args : pattern option ref;
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
  | Tuple of {
      unnamed_fields : pattern list;
      named_fields : pattern StringMap.t;
      data : 'data;
    }
  | Variant of { name : string; value : pattern option; data : 'data }
  | Union of { a : pattern; b : pattern; data : 'data }

and pattern_data = type_inference_data
and pattern = pattern_data pattern_node

(* todo GADT? *)
and expanded_macro = Compiled of compiled | Pattern of pattern
and evaled = { value : value; new_bindings : local StringMap.t }
and compiled = { ir : ir; new_bindings : local StringMap.t }
and struct' = { parent : struct' option; mutable data : state_data }
and contexts = value list Id.Map.t
and contexts_type = int Id.Map.t

and state = {
  self : struct';
  data : state_data;
  contexts : contexts;
  builtins : value StringMap.t;
}

and state_data = { locals : local StringMap.t; syntax : Syntax.syntax }
and local = { mutable value : value; binding : binding }
and binding = { id : id; name : string; value_type : inference_var; mut : bool }

and inference_data = {
  mutable inferred : value option;
  mutable checks : (value -> bool) list;
  mutable type_var : inference_var option;
}

and inference_var_data = Root of inference_data | SameAs of inference_var
and inference_var = { mutable data : inference_var_data; id : Id.t }

let ir_data : 'data. 'data ir_node -> 'data = function
  | Use { data; _ }
  | MultiSet { data; _ }
  | NewType { data; _ }
  | ConstructVariant { data; _ }
  | Builtin { data; _ }
  | Assign { data; _ }
  | Void { data; _ }
  | Struct { data; _ }
  | CheckImpl { data; _ }
  | CreateImpl { data; _ }
  | GetImpl { data; _ }
  | Match { data; _ }
  | OneOf { data; _ }
  | Scope { data; _ }
  | UnwindableBlock { data; _ }
  | WithContext { data; _ }
  | CurrentContext { data; _ }
  | TypeOf { data; _ }
  | TypeOfValue { data; _ }
  | Instantiate { data; _ }
  | Template { data; _ }
  | Function { data; _ }
  | Tuple { data; _ }
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

and pattern_data (pattern : pattern) : pattern_data =
  match pattern with
  | Placeholder { data; _ }
  | Variant { data; _ }
  | Union { data; _ }
  | Void { data; _ }
  | Binding { data; _ }
  | Tuple { data; _ } ->
      data
