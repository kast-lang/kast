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

  and 'data match_branch = { pattern : pattern; body : ir }
  and 'data get_impl = { captured : state; ty : ir; trait : ir; data : 'data }

  and 'data ir_node =
    | Void of { data : 'data }
    | Struct of { body : ir; data : 'data }
    | CreateImpl of {
        captured : state;
        ty : ir;
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
    (* todo remove? *)
    | Instantiate of {
        captured : state;
        template : ir;
        args : ir;
        data : 'data;
      }
    | BuiltinFn of { f : builtin_fn; data : 'data }
    | If of { cond : ir; then_case : ir; else_case : ir; data : 'data }
    | Let of { pattern : pattern; value : ir; data : 'data }

  and inference_status = NotYet | InProgress | Done
  and type_inference_data = { var : inference_var }
  and no_data = NoData
  and ir_data = type_inference_data
  and ir = ir_data ir_node
  and fn_result_type = Ast of ast | Actual of value_type

  and fn = {
    where_clause : ast option;
    captured : state;
    args_pattern : ast option;
    result_type : fn_result_type option;
    contexts : ast option;
    vars : fn_type_vars;
    body : ast;
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
  and compiled = { ir : ir; new_bindings : state_local StringMap.t }
  and struct' = { parent : struct' option; mutable data : state_data }
  and contexts = (value_type, value list) Hashtbl.t
  and contexts_type = (value_type, int) Hashtbl.t
  and state = { self : struct'; data : state_data; contexts : contexts }
  and state_data = { locals : state_local StringMap.t; syntax : Syntax.syntax }
  and state_local = Value of value | Binding of binding
  and binding = { name : string; value_type : inference_var }

  exception Unwind of id * value

  let empty_contexts : contexts = Hashtbl.create 0
  let empty_contexts_type : contexts_type = Hashtbl.create 0
  let default_contexts_type : contexts_type = empty_contexts_type

  let trait_impls : (value_type, (value_type, value) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 0

  let empty_type_var_map () : type_var_map ref = ref Id.Map.empty

  exception FailedUnite of string

  let failinfer () = raise @@ FailedUnite "inference union failed"

  let rec _rec_block_start () =
    failwith "this is here to easier finding functions by 'and name' KEKW"

  and inference_unite_contexts (a : contexts_type) (b : contexts_type) :
      contexts_type =
    if a = b then a else failinfer ()

  and inference_unite_types (a : value_type) (b : value_type) : value_type =
    match (a, b) with
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
    | Void, Void -> Void
    | Void, _ -> failinfer ()
    | Any, Any -> Any
    | Any, _ -> failinfer ()
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
    | SpecificType _, _ -> failwith "inferred specific type"
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
    | Template _, _ -> failwith "todo inferred template type?"
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
    | Var a, Var b -> if a = b then Var a else failinfer ()
    | Var _, _ -> failinfer ()
    | MultiSet _, _ -> failwith "todo inferred multiset"

  and inference_unite (a : value) (b : value) : value =
    match (a, b) with
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
    MyInference.set (ir_data ir).var (Type t : value)

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
    var_type (pattern_data pattern).var

  and ir_type (ir : ir) : value_type = var_type (ir_data ir).var

  and set_pattern_type (pattern : pattern) (t : value_type) =
    MyInference.set (pattern_data pattern).var (Type t : value)

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
    let known value =
      let var = MyInference.new_var () in
      MyInference.set var value;
      { var }
    in
    let known_type t = known (Type t : value) in
    let unknown () = { var = MyInference.new_var () } in
    let same_as other = { var = (ir_data other).var } in
    match ir with
    | Void { data = NoData } -> Void { data = known_type Void }
    | Const { value; data = NoData } ->
        Const { value; data = known_type @@ type_of_value ~ensure:false value }
    | Struct { body; data = NoData } -> Struct { body; data = same_as body }
    | CreateImpl { captured; ty; trait = trait_ir; impl; data = NoData } ->
        let trait = eval_ir captured trait_ir in
        set_ir_type ty Type;
        set_ir_type impl @@ value_to_type trait.value;
        CreateImpl
          { captured; ty; trait = trait_ir; impl; data = known_type Void }
    | GetImpl { captured; ty; trait = trait_ir; data = NoData } ->
        let trait = eval_ir captured trait_ir in
        set_ir_type ty Type;
        GetImpl
          {
            captured;
            ty;
            trait = trait_ir;
            data = known_type @@ value_to_type trait.value;
          }
    | CheckImpl { captured; ty; trait; data = NoData } ->
        set_ir_type ty Type;
        CheckImpl { captured; ty; trait; data = known_type Bool }
    | Match { value; branches; data = NoData } ->
        let value_var = (ir_data value).var in
        let var = MyInference.new_var () in
        List.iter
          (fun { pattern; body } ->
            MyInference.make_same var (ir_data body).var;
            MyInference.make_same value_var (pattern_data pattern).var)
          branches;
        Match { value; branches; data = { var } }
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
        MyInference.make_same (pattern_data pattern).var (ir_data value).var;
        Let { data = known_type Void; pattern; value }
    | Discard { data = NoData; value } ->
        Discard { data = known_type Void; value }
    | If { data = NoData; cond; then_case; else_case } ->
        set_ir_type cond Bool;
        MyInference.make_same (ir_data then_case).var (ir_data else_case).var;
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
        Unwinding { data = { var = f_type.result_type }; f }
    | Call { data = NoData; f; args } ->
        let f_type = new_fn_type_vars () in
        MyInference.make_same f_type.arg_type (ir_data args).var;
        set_ir_type f @@ Fn (fn_type_vars_to_type f_type);
        Call { data = { var = f_type.result_type }; f; args }
    | Then { data = NoData; first; second } ->
        set_ir_type first Void;
        Then { data = same_as second; first; second }
    | Binding { data = NoData; binding } ->
        Binding { data = { var = binding.value_type }; binding }
    | Function { data = NoData; f } ->
        Function { data = known_type @@ Fn (fn_type_vars_to_type f.vars); f }
    | Template { data = NoData; f } ->
        Template
          { data = known_type @@ Template (template_to_template_type f); f }
    | WithContext { data = NoData; new_context; expr } ->
        WithContext { data = same_as expr; new_context; expr }
    | CurrentContext { data = NoData; context_type } ->
        CurrentContext { data = known_type context_type; context_type }
    | FieldAccess { data = NoData; obj; name; default_value } ->
        (* todo
           Inference.expand_dict obj.var name?
        *)
        let var = MyInference.new_var () in
        (match default_value with
        | Some default -> MyInference.make_same var (ir_data default).var
        | None -> ());
        FieldAccess { data = { var }; obj; name; default_value }
    | BuiltinFn { data = NoData; f } ->
        BuiltinFn
          {
            data =
              known_type
              @@ Fn
                   {
                     arg_type = f.arg_type;
                     result_type = f.result_type;
                     contexts = f.contexts;
                   };
            (* TODO because multitarget? data = unknown (); *)
            f;
          }
    | Instantiate { data = NoData; captured; template; args } ->
        Instantiate { (* TODO *) data = unknown (); captured; template; args }

  and init_pattern (p : no_data pattern_node) : pattern =
    let known value =
      let var = MyInference.new_var () in
      MyInference.set var value;
      { var }
    in
    let known_type t = known (Type t : value) in
    let unknown () = { var = MyInference.new_var () } in
    let same_as other = { var = (pattern_data other).var } in
    match p with
    | Void { data = NoData } -> Void { data = known_type Void }
    | Binding { data = NoData; binding } ->
        Binding { data = { var = binding.value_type }; binding }
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
        MyInference.make_same (pattern_data a).var (pattern_data b).var;
        Union { data = same_as a; a; b }

  and show : value -> string = function
    | Ast ast -> "`(" ^ Ast.show ast ^ ")"
    | Variant { name; value; _ } ->
        name ^ show_or "" (fun value -> " " ^ show value) value
    | UnwindToken id -> "unwind token " ^ Id.show id
    | Void -> "void"
    | Macro f -> "macro " ^ show_fn f
    | BuiltinMacro _ -> "builtin_macro"
    | BuiltinFn { name; _ } -> "builtin_fn " ^ name
    | Template f -> "template " ^ show_fn f
    | Function f -> "function" ^ show_fn f
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

  and show_fn (f : fn) : string =
    (match f.args_pattern with Some ast -> Ast.show ast | None -> "()")
    ^ " => " ^ Ast.show f.body

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
    | InferVar var -> (
        match (MyInference.get_inferred var : value option) with
        | Some (Type inferred) -> show_type inferred
        | Some _ -> failwith "type was inferred as not a type wtf"
        | None -> "<not inferred>")
    | MultiSet _ -> failwith "todo show multiset"

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
    | Template { f; _ } -> "template " ^ show_fn f
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
              (if acc = "" then "" else acc ^ ", ")
              ^ name ^ ": " ^ show_ir field)
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
        "(if " ^ show_ir cond ^ " " ^ show_ir then_case ^ " "
        ^ show_ir else_case ^ ")"
    | Let { pattern; value; _ } ->
        "(let " ^ show_pattern pattern ^ " " ^ show_ir value ^ ")"

  and show_pattern : pattern -> string = function
    | Void _ -> "()"
    | Union { a; b; _ } -> show_pattern a ^ " | " ^ show_pattern b
    | Binding { binding; _ } -> binding.name
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

  and unwrap_specifics : value_type -> value_type = function
    | SpecificType t -> unwrap_specifics t
    | t -> t

  and type_of_fn (f : fn) ~(ensure : bool) : fn_type =
    if ensure then (
      Log.info "getting type of fun";
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
          arg_type = inferred_type (pattern_data compiled.args).var;
          contexts = compiled.contexts;
        }

  and template_to_template_type (f : fn) : fn =
    {
      vars =
        {
          arg_type = f.vars.arg_type;
          contexts = f.vars.contexts;
          result_type = MyInference.new_var ();
        };
      args_pattern = f.args_pattern;
      where_clause = f.where_clause;
      result_type = None;
      captured = f.captured;
      contexts = f.contexts;
      body =
        Complex
          {
            def =
              { name = "typeofvalue"; assoc = Left; priority = 0; parts = [] };
            values = StringMap.singleton "expr" f.body;
            data = Ast.data f.body;
          };
      compiled = None;
    }

  and type_of_value (value : value) ~(ensure : bool) : value_type =
    match value with
    | Ast _ -> Ast
    | UnwindToken _ -> UnwindToken
    | Void -> Void
    | Variant { typ; _ } -> typ
    | BuiltinMacro _ -> BuiltinMacro
    | BuiltinFn { arg_type; result_type; contexts; _ } ->
        Fn { arg_type; result_type; contexts }
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
                (fun (local : state_local) ->
                  match local with
                  | Binding _ -> failwith "binding wtf"
                  | Value value -> type_of_value ~ensure value)
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

  and just_value value = { value; new_bindings = StringMap.empty }

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
        let rec find_in_scopes (s : struct') =
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
                    if new_bindings then
                      Binding
                        {
                          binding =
                            {
                              name = ident;
                              value_type = MyInference.new_var ();
                            };
                          data = NoData;
                        }
                      |> init_ir
                    else (
                      log_state Log.Debug self;
                      failwith (ident ^ " not found in current scope"))
                | Some (Value value) ->
                    Const { value; data = NoData } |> init_ir
                | Some (Binding binding) ->
                    Binding { binding; data = NoData } |> init_ir)
            | Number raw -> Number { raw; data = NoData } |> init_ir
            | String { value; raw } ->
                String { value; raw; data = NoData } |> init_ir
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
                      { name = ident; value_type = MyInference.new_var () };
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
    | Variant { data; _ }
    | Union { data; _ }
    | Void { data; _ }
    | Binding { data; _ }
    | Dict { data; _ } ->
        data

  and pattern_bindings (pattern : pattern) : binding StringMap.t =
    match pattern with
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
                  data = NoData;
                }
              |> init_ir
            in
            Compiled
              {
                ir =
                  Call
                    {
                      f = BuiltinFn { f; data = NoData } |> init_ir;
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
    let compiled = compile_ast_to_ir self ast ~new_bindings:false in
    Log.trace ("compiled: " ^ show_ir compiled.ir);
    eval_ir self compiled.ir

  and log_state (level : Log.level) (self : state) : unit =
    let log = Log.with_level level in
    log "locals:";
    StringMap.iter
      (fun name local ->
        log
          ("  " ^ name ^ " = "
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

  and check_impl (self : state) ({ trait; ty; _ } : 'data get_impl) : bool =
    let trait = value_to_type (eval_ir self trait).value in
    let ty = value_to_type (eval_ir self ty).value in
    match Hashtbl.find_opt trait_impls ty with
    | Some impls -> (
        match Hashtbl.find_opt impls trait with Some _ -> true | None -> false)
    | None -> false

  and get_impl (self : state) ({ trait; ty; _ } : 'data get_impl) : value =
    let trait = value_to_type (eval_ir self trait).value in
    let ty = value_to_type (eval_ir self ty).value in
    let show_impls (ty : value_type) (impls : (value_type, value) Hashtbl.t) =
      Hashtbl.iter
        (fun trait impl ->
          Log.trace @@ "impl " ^ show_type trait ^ " for " ^ show_type ty
          ^ " as " ^ show impl)
        impls
    in
    let show_all_impls () = Hashtbl.iter show_impls trait_impls in
    let fail s =
      (* show_all_impls (); *)
      (* Log.trace s; *)
      failwith s
    in
    match Hashtbl.find_opt trait_impls ty with
    | Some impls -> (
        match Hashtbl.find_opt impls trait with
        | Some impl -> impl
        | None ->
            fail @@ "get_impl failed: " ^ show_type trait
            ^ " is not implemented for " ^ show_type ty
            ^ " (see existing impls above)")
    | None ->
        fail @@ "get_impl failed: " ^ show_type trait
        ^ " is not implemented for " ^ show_type ty ^ " (no impls found)"

  and eval_ir (self : state) (ir : ir) : evaled =
    log_state Log.Never self;
    let result_type = inferred_type (ir_data ir).var in
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
                   {
                     syntax = Syntax.empty;
                     locals =
                       StringMap.map
                         (fun value : state_local -> Value value)
                         evaled.new_bindings;
                   };
               })
      | NewType { def; _ } ->
          just_value (Type (NewType (inferred_type (pattern_data def).var)))
      | Scope { expr; _ } -> just_value (eval_ir self expr).value
      | CreateImpl { trait; ty; impl; _ } ->
          let trait = value_to_type (eval_ir self trait).value in
          let ty = value_to_type (eval_ir self ty).value in
          let impl = (eval_ir self impl).value in
          if Hashtbl.find_opt trait_impls ty |> Option.is_none then
            Hashtbl.add trait_impls ty (Hashtbl.create 0);
          let type_impls = Hashtbl.find trait_impls ty in
          Hashtbl.add type_impls trait impl;
          Log.trace @@ "added impl " ^ show_type trait ^ " for " ^ show_type ty
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
                                 update_locals self.data.locals
                                 @@ StringMap.map
                                      (fun value -> Value value)
                                      new_bindings;
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
          just_value (Type (inferred_type (ir_data expr).var))
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
          just_value (Template { f with captured = self; compiled = None })
      | Function { f; _ } ->
          just_value (Function { f with captured = self; compiled = None })
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
                    failwith ("field " ^ name ^ " does not exist in " ^ show obj)
                )
          in
          just_value value
      | Const { value; _ } -> just_value value
      | Binding { binding; _ } -> (
          match get_local_value_opt self binding.name with
          | None -> failwith (binding.name ^ " not found wtf, we are compiled")
          | Some value -> just_value value)
      | Number { raw = s; data } ->
          just_value
            (match inferred_type data.var with
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
                    locals =
                      update_locals self.data.locals
                        (StringMap.mapi
                           (fun _name value ->
                             Log.trace (_name ^ " = " ^ show value);
                             Value value)
                           first.new_bindings);
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
          (* todo memoization *)
          just_value (f args)
      | Call { f; args; _ } ->
          let f = (eval_ir self f).value in
          Log.trace ("calling " ^ show f);
          let (make_f, vars) : (unit -> value -> value) * fn_type_vars =
            match f with
            | BuiltinFn f ->
                ( (fun () -> f.impl),
                  {
                    arg_type = type_into_var f.arg_type;
                    result_type = type_into_var f.result_type;
                    contexts = contexts_type_into_var f.contexts;
                  } )
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
                                       "builtin macro arg must be dict of asts")
                               fields)
                            ~new_bindings:false
                        with
                        | Compiled { ir; _ } -> ir
                        | Pattern _ -> failwith "wtf"
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
          let args = (eval_ir self args).value in
          let f = make_f () in
          just_value (f args)
      | If { cond; then_case; else_case; _ } ->
          let cond = eval_ir self cond in
          let self_with_new_bindings =
            {
              self with
              data =
                {
                  self.data with
                  locals =
                    update_locals self.data.locals
                      (StringMap.map
                         (fun value -> Value value)
                         cond.new_bindings);
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
      Log.debug ("compiling " ^ Ast.show f.body);
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
               | None -> Const { value = Bool true; data = NoData } |> init_ir
               | Some clause ->
                   (compile_ast_to_ir captured clause ~new_bindings:false).ir);
             args;
             result_type = InferVar f.vars.result_type;
             result_type_ir =
               (match f.result_type with
               | None -> None
               | Some (Actual t) ->
                   Some (Const { value = Type t; data = NoData } |> init_ir)
               | Some (Ast ast) ->
                   Some (compile_ast_to_ir captured ast ~new_bindings:false).ir);
             body = (compile_ast_to_ir captured f.body ~new_bindings:false).ir;
             contexts =
               (match f.contexts with
               | Some contexts ->
                   value_to_contexts_type (eval_ast f.captured contexts).value
               | None -> default_contexts_type);
           }));
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
                     Value value)
                   (pattern_match f.args args));
          };
      }
    in
    (match (eval_ir captured f.where_clause).value with
    | Bool true -> ()
    | Bool false ->
        failwith @@ "where clause failed: " ^ show_ir f.where_clause
        ^ ", args = " ^ show args
    | value ->
        failwith @@ "where clause evaluated to " ^ show value
        ^ ", expected a bool");
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
    | Type (OneOf variants as typ) ->
        Option.map
          (fun (variant : value_type option) : value ->
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
              value_to_type (eval_ast self (StringMap.find "type" args)).value
            in
            let var =
              match value with
              | Compiled compiled -> (ir_data compiled.ir).var
              | Pattern pattern -> (pattern_data pattern).var
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
                let f = compile_ast_to_ir self f ~new_bindings in
                let args = compile_ast_to_ir self args ~new_bindings in
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
              | Some b ->
                  compile_ast_to_ir self_with_new_bindings b ~new_bindings
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
                          data = NoData;
                        }
                      |> init_ir;
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
            let rec impl : ast -> ir = function
              | Complex { def = { name = "unquote"; _ }; values; _ } ->
                  let inner = StringMap.find "expr" values in
                  Log.trace ("unquoting" ^ Ast.show inner);
                  (compile_ast_to_ir self inner ~new_bindings).ir
              | Nothing data ->
                  Const { value = Ast (Nothing data); data = NoData } |> init_ir
              | Simple token ->
                  Const { value = Ast (Simple token); data = NoData } |> init_ir
              | Complex { def; values; data = ast_data } ->
                  Ast
                    {
                      def;
                      values = StringMap.map impl values;
                      ast_data;
                      data = NoData;
                    }
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
            let value = (compile_ast_to_ir self value ~new_bindings).ir in
            Compiled
              {
                ir = Let { pattern; value; data = NoData } |> init_ir;
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
                let evaled = eval_ir self let_ir in
                Compiled
                  {
                    ir = let_ir;
                    new_bindings =
                      StringMap.map
                        (fun value -> Value value)
                        evaled.new_bindings;
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
                          vars = new_fn_type_vars ();
                          captured = self;
                          where_clause = where;
                          result_type = Some (Actual Any);
                          args_pattern = Some args;
                          body = def;
                          contexts = None;
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
                          vars = new_fn_type_vars ();
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
                      Dict
                        {
                          fields =
                            StringMap.singleton name
                              (compile_ast_to_ir self value ~new_bindings).ir;
                          data = NoData;
                        }
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
            let expr = StringMap.find "expr" args in
            Compiled
              {
                ir =
                  TypeOf
                    {
                      captured = self;
                      expr = (compile_ast_to_ir self expr ~new_bindings).ir;
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
            let expr = StringMap.find "expr" args in
            Compiled
              {
                ir =
                  TypeOfValue
                    {
                      captured = self;
                      expr = (compile_ast_to_ir self expr ~new_bindings).ir;
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
                           data = NoData;
                         }
                      |> init_pattern)
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
                              data = NoData;
                            }
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
                          Dict { fields = fields a; data = NoData } |> init_ir;
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
            print_endline
              (show value ^ " : "
              ^ show_type (type_of_value ~ensure:false value));
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
              let result : value =
                Type (Fn { arg_type; result_type; contexts })
              in
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
                   (Random.int32
                      (Int32.sub (Int32.add max (Int32.of_int 1)) min)))
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
                      data = NoData;
                    }
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
                  (fun typ -> (compile_ast_to_ir self typ ~new_bindings).ir)
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
                      data = NoData;
                    }
                  |> init_ir;
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
                ir = Struct { body; data = NoData } |> init_ir;
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
            Log.trace "making new type var";
            Type (Var (Id.gen ())));
      }

    let all_list : (string * value) list =
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
              impl = (fun x -> Type (type_of_value ~ensure:true x));
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

    let all : value StringMap.t = StringMap.of_list all_list
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
                   (StringMap.singleton "Self" (Struct self : value)
                     : value StringMap.t));
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
    let result = eval_ast !self ast in
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
end

and Checker : Inference.Checker = struct
  type t = Impl.value

  let unite = Impl.inference_unite
end

and MyInference : (Inference.T with type inferred := Impl.value) =
  Inference.Make (Checker)
