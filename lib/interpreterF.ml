open Prelude
open Types

module Make
    (Compiler : Modules.Compiler)
    (Show : Modules.Show)
    (Utils : Modules.Utils)
    (Inference : Modules.Inference)
    (Builtins : Modules.Builtins)
    (TypeId : Modules.TypeId)
    (Cast : Modules.Cast) =
struct
  open Show
  open Utils
  open Compiler

  exception Unwind of id * value

  let empty_contexts : contexts = Id.Map.empty
  let empty_contexts_type : contexts_type = Id.Map.empty
  let default_contexts_type : contexts_type = empty_contexts_type

  let rec just_value value = { value; new_bindings = StringMap.empty }

  and pattern_match_opt (pattern : pattern) (value : value) :
      local StringMap.t option =
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
      | Binding { binding; _ } ->
          Some (StringMap.singleton binding.name { value; binding })
      | Dict { fields = field_patterns; _ } -> (
          match value with
          | InferVar var ->
              let dict =
                (Dict
                   {
                     fields =
                       field_patterns
                       |> StringMap.map (fun _ : value ->
                              InferVar (Inference.new_var ()));
                   }
                  : value)
              in
              Inference.set var dict;
              pattern_match_opt pattern dict
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

  and pattern_match (pattern : pattern) (value : value) : local StringMap.t =
    match pattern_match_opt pattern value with
    | Some result -> result
    | None ->
        Log.error @@ " while pattern matching " ^ show value ^ " with "
        ^ show_pattern pattern;
        failwith "match failed"

  and get_local_opt (self : state) (name : string) : local option =
    (* Log.trace @@ "getting local " ^ name; *)
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
    match get_local_opt self name with
    | Some { value = Ref value; _ } -> Some !value
    | Some { value; _ } -> Some value
    | None -> (
        match strip_prefix ~prefix:"builtin " name with
        | Some builtin_name ->
            (* TODO builtins should not even be looked up here *)
            StringMap.find_opt builtin_name self.builtins
        | None -> None)

  and call_compiled (current_contexts : contexts) (f : compiled_fn)
      (args : value) : value =
    let args_matches =
      StringMap.mapi
        (fun name local ->
          Log.trace ("called with arg " ^ name ^ " = " ^ show local.value);
          local)
        (pattern_match f.args args)
    in
    let captured =
      {
        f.captured with
        data =
          {
            f.captured.data with
            locals = update_locals f.captured.data.locals args_matches;
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
    try (eval_ir { captured with contexts = current_contexts } f.body).value
    with Failure _ as failure ->
      Log.error @@ "  while calling with args = " ^ show args;
      raise failure

  and discard : value -> unit = function
    | Void -> ()
    | that ->
        failwith ("only void can be discarded (discarded " ^ show that ^ ")")

  and get_field_opt (obj : value) (field : string) : value option =
    match obj with
    | Dict { fields = dict } -> StringMap.find_opt field dict
    | Struct s ->
        StringMap.find_opt field s.data.locals
        |> Option.map (fun local -> local.value)
    | Type (Dict { fields }) ->
        StringMap.find_opt field fields |> Option.map (fun t : value -> Type t)
    | Type (OneOf variants as typ) ->
        Option.map
          (fun (variant : value_type option) : value ->
            match variant with
            | Some variant ->
                BuiltinFn
                  {
                    f =
                      {
                        name = "type constructor";
                        impl =
                          (fun _fn_type value ->
                            Variant { name = field; typ; value = Some value });
                      };
                    ty =
                      Some
                        {
                          arg_type = variant;
                          result_type = typ;
                          contexts = empty_contexts_type;
                        };
                  }
            | None -> Variant { name = field; typ; value = None })
          (StringMap.find_opt field variants)
    | _ -> failwith @@ "can't get field " ^ field ^ " of " ^ show obj

  and empty_state () : state =
    let self =
      {
        parent = None;
        data = { locals = StringMap.empty; syntax = Syntax.empty };
      }
    in
    let state =
      {
        self;
        data = { locals = StringMap.empty; syntax = Syntax.empty };
        builtins = Builtins.all ();
        contexts = Id.Map.empty;
      }
    in
    state

  and std_path () : string =
    Sys.getenv_opt "KAST_STD" |> Option.value ~default:"std"

  and default_state () : state =
    let std =
      eval_file (only_std_syntax () |> ref) ~filename:(std_path () ^ "/lib.ks")
    in
    let state = only_std_syntax () |> ref in
    state :=
      {
        !state with
        data =
          {
            !state.data with
            locals =
              update_locals !state.data.locals
                (StringMap.singleton "std"
                   {
                     value = std;
                     binding =
                       {
                         id = Id.gen ();
                         name = "std";
                         mut = false;
                         value_type =
                           Inference.new_set_var
                           @@ Type (type_of_value ~ensure:false std);
                       };
                   });
          };
      };
    !state

  and only_std_syntax () : state =
    let s = ref (empty_state ()) in
    ignore @@ eval_file s ~filename:(std_path () ^ "/syntax.ks");
    !s

  and get_ast (self : state ref) (s : string) ~(filename : string) : ast =
    let filename = Span.Filename filename in
    !self.self.data <- !self.data;
    let tokens = Lexer.parse s filename in
    let ast = Ast.parse !self.data.syntax tokens filename in
    Log.trace (Ast.show ast);
    Ast.map (fun span -> { span }) ast

  and compile (self : state ref) (s : string) ~(filename : string) : compiled =
    try
      let ast = get_ast self s ~filename in
      compile_ast_to_ir !self ast
    with Failure _ as failure ->
      Log.error @@ "  while compiling " ^ filename;
      raise failure

  and compile_file (self : state ref) ~(filename : string) : compiled =
    let f = open_in filename in
    let contents = really_input_string f (in_channel_length f) in
    close_in f;
    compile self contents ~filename

  and eval (self : state ref) (s : string) ~(filename : string) : value =
    try
      let ast = get_ast self s ~filename in
      let compiled = compile_ast_to_ir !self ast in
      let result : evaled = eval_ir !self compiled.ir in
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
    with Failure _ as failure ->
      Log.error @@ "  while evaluating file " ^ filename;
      raise failure

  and eval_file (self : state ref) ~(filename : string) : value =
    let f = open_in filename in
    let contents = really_input_string f (in_channel_length f) in
    close_in f;
    let value = eval self contents ~filename in
    Log.trace ("after " ^ filename ^ " syntax:");
    Log.trace (Syntax.show !self.data.syntax);
    value

  and eval_call (f : value) (args : value) (contexts : contexts) : value =
    Log.trace @@ "calling " ^ show f ^ " (" ^ show args ^ ")";
    let f_impl : value -> value =
      match f with
      | BuiltinFn f -> (
          fun args ->
            try
              f.f.impl
                (match f.ty with
                | Some t -> t
                | None -> new_fn_type_vars () |> fn_type_vars_to_type)
                args
            with Failure _ as failure ->
              Log.error @@ "  while calling builtin fn " ^ f.f.name;
              raise failure)
      | Function f | Macro f -> call_compiled contexts @@ ensure_compiled f
      | _ -> failwith @@ show f ^ " - not a function"
    in
    Log.trace ("calling " ^ show f);
    Log.trace @@ "args = " ^ show args;
    let result = f_impl args in
    Log.trace @@ "result = " ^ show result;
    result

  and eval_ast (self : state) (ast : ast) : evaled =
    let compiled = compile_ast_to_ir self ast in
    Log.trace ("compiled: " ^ show_ir compiled.ir);
    eval_ir self compiled.ir

  and log_state : Log.level -> state -> unit =
   fun (level : Log.level) (self : state) : unit ->
    let log = Log.with_level level in
    log "locals:";
    StringMap.iter
      (fun name { value; _ } -> log ("  " ^ name ^ " = " ^ show value))
      self.data.locals

  and namespace_locals (namespace : value) : local StringMap.t =
    match namespace with
    | Struct s -> s.data.locals
    | Dict { fields } ->
        fields
        |> StringMap.mapi (fun name value : local ->
               {
                 value;
                 binding =
                   {
                     id = Id.gen ();
                     name;
                     value_type =
                       Inference.new_set_var
                         (Type (type_of_value ~ensure:false value));
                     mut = false;
                   };
               })
    | _ -> failwith @@ "this is not a namespace: " ^ show namespace

  and eval_ir (self : state) (ir : ir) : evaled =
    try
      log_state Log.Never self;
      let result_type =
        Inference.get_inferred_as_type (ir_data ir).inference.type_var
      in
      Log.trace
        ("evaluating " ^ show_ir ir ^ " inferred as " ^ show_type @@ result_type);
      (* forward_expected_type ir expected_type; *)
      (* let result_type = (ir_data ir).result_type in *)
      let result =
        match ir with
        | Void _ -> just_value Void
        | MultiSet { a; b; _ } ->
            let into_set : value -> value list = function
              | MultiSet s -> s
              | other -> [ other ]
            in
            let a = (eval_ir self a).value |> into_set in
            let b =
              match b with
              | Some b -> (eval_ir self b).value |> into_set
              | None -> []
            in
            just_value @@ MultiSet (a @ b)
        | Use { namespace; _ } ->
            { value = Void; new_bindings = namespace_locals namespace }
        | Struct { body; _ } ->
            Log.trace "evaluating struct";
            let struct' : struct' =
              {
                parent = Some self.self;
                data = { syntax = Syntax.empty; locals = StringMap.empty };
              }
            in
            let evaled =
              eval_ir
                {
                  self = struct';
                  data = self.data;
                  contexts = self.contexts;
                  builtins = self.builtins;
                }
                body
            in
            (* needs refactored *)
            (* feels refactored *)
            struct'.data <- { struct'.data with locals = evaled.new_bindings };
            just_value (Struct struct')
        | NewType { def; _ } ->
            just_value
            @@ Type
                 (let variants =
                    match (eval_ir self def).value with
                    | Variant _ as value -> [ value ]
                    | MultiSet variants -> variants
                    | other -> failwith @@ "todo newtype of " ^ show other
                  in
                  OneOf
                    (StringMap.of_list
                    @@ List.map
                         (fun (value : value) ->
                           match value with
                           | Variant { typ; name; value } ->
                               (name, value |> Option.map value_to_type)
                           | _ -> failwith @@ "should be multiset of variants")
                         variants))
        | Scope { expr; _ } -> just_value (eval_ir self expr).value
        | Assign { pattern; value; _ } ->
            let value = (eval_ir self value).value in
            pattern_match pattern value
            |> StringMap.iter (fun name { value; _ } ->
                   let local =
                     match get_local_opt self name with
                     | Some local -> local
                     | None -> failwith @@ name ^ " not found"
                   in
                   if not local.binding.mut then
                     failwith @@ name ^ " is not mut";
                   local.value <- value);
            { value = Void; new_bindings = StringMap.empty }
        | CreateImpl { trait; value; impl; _ } ->
            let ty = value_to_type (eval_ir self value).value in
            let trait = (eval_ir self trait).value in
            let impl = (eval_ir self impl).value in
            Cast.add_rule (Type ty) ~trait ~impl;
            just_value Void
        | GetImpl { trait; value; _ } ->
            let trait = (eval_ir self trait).value in
            let value = (eval_ir self value).value in
            just_value (Cast.perform value ~trait)
        | CheckImpl { trait; value; _ } ->
            let trait = (eval_ir self trait).value in
            let value = (eval_ir self value).value in
            just_value (Bool (Cast.check value ~trait))
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
        | ConstructVariant { variant; value; _ } ->
            just_value
            @@ Variant
                 {
                   typ = result_type;
                   name = variant;
                   value =
                     value
                     |> Option.map (fun value -> (eval_ir self value).value);
                 }
        | OneOf { variants; _ } ->
            just_value
              (Type
                 (OneOf
                    (StringMap.map
                       (Option.map (fun variant ->
                            value_to_type @@ (eval_ir self variant).value))
                       variants)))
        | TypeOf { expr; _ } ->
            let inferred_type =
              Inference.get_inferred_as_type (ir_data expr).inference.type_var
              |> substitute_type_bindings (get_local_value_opt self)
            in
            just_value (Type inferred_type)
        | TypeOfValue { expr; _ } ->
            just_value
              (Type (type_of_value ~ensure:true (eval_ir self expr).value))
            (* Is there anything that works? *)
        | Builtin { name; data } ->
            let inferred_type =
              Inference.get_inferred_as_type data.inference.type_var
            in
            Log.trace @@ "builtin " ^ name ^ " inferred as "
            ^ show_type inferred_type;
            let value =
              match StringMap.find name self.builtins with
              | BuiltinFn { f; ty = _ } ->
                  (BuiltinFn
                     {
                       f;
                       ty =
                         (match inferred_type with
                         | Fn f -> Some f
                         | _ ->
                             failwith
                               "builtin fn was inferred to be not a fn ???");
                     }
                    : value)
              | ( Ir _ | Binding _ | Var _ | InferVar _ | UnwindToken _
                | DelimitedToken _ | Ast _ | Macro _ | BuiltinMacro _
                | Template _ | Function _ | Void | Bool _ | Int32 _ | Int64 _
                | Float64 _ | String _ | Dict _ | Struct _ | Ref _ | Type _
                | Variant _ | MultiSet _ ) as other ->
                  other
            in
            Log.trace @@ "builtin " ^ name ^ " = " ^ show value ^ " :: "
            ^ show_type (type_of_value ~ensure:false value);
            just_value value
        | BuiltinFn { f; _ } ->
            just_value
              (* TODO setup type properly
                 although this ir is to be deleted?
              *)
              (BuiltinFn { f; ty = None })
        | UnwindableBlock { f; _ } ->
            just_value
              (match (eval_ir self f).value with
              | Function f -> (
                  let compiled = ensure_compiled f in
                  let token = Id.gen () in
                  try call_compiled self.contexts compiled (UnwindToken token)
                  with Unwind (unwinded_token, value) ->
                    if unwinded_token = token then value
                    else raise (Unwind (unwinded_token, value)))
              | _ -> failwith "unwindable_block must take a function")
        | WithContext { new_context; expr; _ } ->
            let new_context = (eval_ir self new_context).value in
            let new_state =
              {
                self with
                contexts =
                  (let context_type = type_of_value ~ensure:true new_context in
                   Id.Map.update (TypeId.get context_type)
                     (fun prev ->
                       Some
                         (new_context
                         :: (match prev with Some prev -> prev | None -> [])))
                     self.contexts);
              }
            in
            just_value (eval_ir new_state expr).value
        | CurrentContext { context_type; _ } -> (
            let context_type =
              substitute_type_bindings (get_local_value_opt self) context_type
            in
            let all_current =
              match Id.Map.find_opt (TypeId.get context_type) self.contexts with
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
                   shared_compiled_args = ref None;
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
                   shared_compiled_args = ref None;
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
                        StringMap.mapi
                          (fun name value ->
                            match (eval_ir self value).value with
                            | Ast ast -> ast
                            | other ->
                                failwith @@ name
                                ^ " expected to be an ast, got " ^ show other)
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
            match get_local_value_opt self binding.name with
            | None ->
                log_state Log.Error self;
                failwith (binding.name ^ " not found wtf, we are compiled")
            | Some value -> just_value value)
        | Number { raw = s; data } ->
            just_value
              (match Inference.get_inferred_as_type data.inference.type_var with
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
            (match f with
            | Function f | Macro f -> ensure_compiled f |> ignore
            | _ -> ());
            let args = (eval_ir self args).value in
            just_value @@ eval_call f args self.contexts
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
               (fun name { value; binding } ->
                 log
                 @@ (if binding.mut then " mut " else " ")
                 ^ name ^ " = " ^ show value)
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
      (* Log.error @@ "  while evaluating " ^ show_ir ir; *)
      raise failure

  and substitute_fn_vars (vars : fn_type_vars) (state : state) : fn_type_vars =
    let sub var =
      InferVar var
      |> substitute_type_bindings (get_local_value_opt state)
      |> (fun t : value -> Type t)
      |> Inference.new_set_var
    in
    {
      arg_type = sub vars.arg_type;
      result_type = sub vars.result_type;
      contexts = sub vars.contexts;
    }

  and substitute_type_bindings :
      (string -> value option) -> value_type -> value_type =
   fun sub t ->
    match t with
    | Binding { name; _ } -> (
        match sub name with Some sub -> sub |> value_to_type | None -> t)
    | Var _ -> t
    | UnwindToken -> t
    | DelimitedToken -> t
    | Never -> t
    | Ast -> t
    | Ir -> t
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
    | OneOf variants ->
        OneOf
          (variants
          |> StringMap.map (Option.map @@ substitute_type_bindings sub))
    | Union _ -> failwith @@ "todo Union " ^ show_type t
    | Type -> t
    | InferVar var -> (
        match Inference.get_inferred var with
        | Some (Type t : value) -> t |> substitute_type_bindings sub
        | Some _ -> failwith "inferred as not type wtf"
        | None -> t)
    | MultiSet _ -> failwith @@ "todo MultiSet " ^ show_type t
    | MultiSetOldToRemove _ -> failwith "todo multiset old"

  and substitute_bindings (sub : string -> value option) (value : value) : value
      =
    match value with
    | Binding { name; _ } -> (
        match sub name with Some sub -> sub | None -> value)
    | Var _ -> value
    | InferVar var -> (
        match Inference.get_inferred var with
        | None -> value
        | Some inferred -> inferred |> substitute_bindings sub)
    | UnwindToken _ -> value
    | DelimitedToken _ -> value
    | Ast _ -> value
    | Ir _ -> value
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
    | MultiSet values -> MultiSet (List.map (substitute_bindings sub) values)
end
