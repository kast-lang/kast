open Types
open Prelude

module Make (Inference : Modules.Inference) (TypeId : Modules.TypeId) :
  Modules.Show = struct
  let show_or : 'a. string -> ('a -> string) -> 'a option -> string =
   fun default f opt -> match opt with Some value -> f value | None -> default

  let rec show : value -> string =
   fun value ->
    (if false then
       Log.trace @@ "in "
       ^
       match value with
       | Binding _ -> "binding"
       | Var _ -> "var"
       | Ast _ -> "ast"
       | Ir _ -> "ir"
       | Variant _ -> "variant"
       | UnwindToken _ -> "unwind token"
       | DelimitedToken _ -> "delimited token"
       | Void -> "void"
       | Macro _ -> "macro"
       | BuiltinMacro _ -> "builtin_macro"
       | BuiltinFn _ -> "builtin_fn"
       | Template _ -> "template"
       | Function _ -> "fn"
       | Int32 _ -> "int32"
       | Int64 _ -> "int64"
       | Float64 _ -> "float64"
       | Bool _ -> "bool"
       | String _ -> "string"
       | Tuple _ -> "tuple"
       | Ref _ -> "ref"
       | Struct _ -> "struct"
       | Type _ -> "type"
       | InferVar _ -> "infervar"
       | MultiSet _ -> "multiset"
       | Builtin _ -> "builtin");
    let result = show2 value in
    result

  and show2 : value -> string = function
    | Binding { name; id; _ } -> "<binding " ^ name ^ " " ^ Id.show id ^ ">"
    | Var { id; typ } -> "var " ^ Id.show id ^ " :: " ^ show_type typ
    | Ast ast -> "`(" ^ Ast.show ast ^ ")"
    | Ir ir -> "ir " ^ show_ir ir
    | Variant { name; value; _ } ->
        "." ^ name ^ show_or "" (fun value -> " " ^ show value) value
    | UnwindToken id -> "unwind token " ^ Id.show id
    | DelimitedToken id -> "delimited token " ^ Id.show id
    | Void -> "void"
    | Macro f -> "macro " ^ show_fn f
    | BuiltinMacro _ -> "builtin_macro"
    | BuiltinFn { f = { name; _ }; _ } -> "builtin_fn " ^ name
    | Template f -> "template " ^ show_fn f
    | Function f -> "function " ^ show_fn f
    | Int32 value -> Int32.to_string value
    | Int64 value -> Int64.to_string value
    | Float64 value -> Float.to_string value
    | Bool value -> Bool.to_string value
    | String value -> "\"" ^ String.escaped value ^ "\""
    | Tuple { unnamed_fields; named_fields } ->
        "( "
        ^ List.fold_left
            (fun acc field ->
              (if acc = "" then "" else acc ^ ", ") ^ show field)
            "" unnamed_fields
        ^ StringMap.fold
            (fun name field acc ->
              (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show field)
            named_fields ""
        ^ " )"
    | Ref value -> "ref " ^ show !value
    | Struct _ -> "struct <...>"
    | Type t -> "type " ^ show_type t
    | InferVar var -> (
        match (Inference.get_inferred var : value option) with
        | Some inferred -> show inferred
        | None -> "<not inferred " ^ Inference.show_id var ^ ">")
    | MultiSet values ->
        List.fold_left (fun acc value -> acc ^ " | " ^ show value) "" values
    | Builtin { name; _ } -> "builtin " ^ name

  and show_fn_ast (f : fn_ast) : string =
    (match f.args with Some ast -> Ast.show ast | None -> "()")
    ^ (match f.returns with Some ast -> " -> " ^ Ast.show ast | None -> "")
    ^ (match f.contexts with Some ast -> " with " ^ Ast.show ast | None -> "")
    ^ " => " ^ Ast.show f.body

  and show_fn (f : fn) : string = if false then "<...>" else show_fn_ast f.ast

  and show_fn_type : fn_type -> string =
   fun { arg_type; contexts; result_type } ->
    show_type arg_type ^ " -> " ^ show_type result_type
    ^ Id.Map.fold
        (fun value_type amount acc ->
          (if acc = "" then " with " else ", ")
          ^ Int.to_string amount ^ " of "
          ^ show_type (TypeId.to_type value_type))
        contexts ""

  and show_contexts : contexts -> string =
   fun contexts ->
    Id.Map.fold
      (fun typ values acc ->
        (if acc = "" then "" else ", ")
        ^ (List.length values |> Int.to_string)
        ^ " of "
        ^ show_type (TypeId.to_type typ))
      contexts ""

  and show_type : value_type -> string =
   fun t ->
    (if false then
       Log.trace @@ "in"
       ^
       match t with
       | Binding _ -> "binding"
       | UnwindToken -> "unwind_token"
       | DelimitedToken -> "delimited token"
       | Never -> "!"
       | Ast -> "ast"
       | Ir -> "ir"
       | Void -> "void"
       | Bool -> "bool"
       | Int32 -> "int32"
       | Int64 -> "int64"
       | Float32 -> "float32"
       | Float64 -> "float64"
       | String -> "string"
       | Fn _ -> "fn"
       | Macro _ -> "macro"
       | Template _ -> "template"
       | BuiltinMacro -> "builtin_macro"
       | Tuple _ -> "tuple"
       | Type -> "type"
       | Union _ -> "union"
       | OneOf _ -> "oneof"
       | NewType _ -> "newtype"
       | Var _ -> "var"
       | InferVar _ -> "infervar"
       | MultiSet _ -> "multiset"
       | MultiSetOldToRemove _ -> "multiset"
       | Builtin _ -> "builtin");
    let result = show_type2 t in
    result

  and show_type2 : value_type -> string = function
    | Binding { name; id; _ } -> "<binding " ^ name ^ " " ^ Id.show id ^ ">"
    | UnwindToken -> "unwind_token"
    | DelimitedToken -> "delimited token"
    | Never -> "!"
    | Ast -> "ast"
    | Ir -> "ir"
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
    | Tuple { unnamed_fields; named_fields } ->
        "( "
        ^ List.fold_left
            (fun acc field_type ->
              (if acc = "" then "" else acc ^ ", ") ^ show_type field_type)
            "" unnamed_fields
        ^ StringMap.fold
            (fun name field_type acc ->
              (if acc = "" then "" else acc ^ ", ")
              ^ name ^ ": " ^ show_type field_type)
            named_fields ""
        ^ " )"
    | Type -> "type"
    | Union set ->
        Id.Set.fold
          (fun t acc ->
            (if acc = "" then "" else acc ^ " | ")
            ^ show_type (TypeId.to_type t))
          set ""
    | OneOf variants ->
        StringMap.fold
          (fun name t acc ->
            (if acc = "" then "" else acc ^ " | ")
            ^ "." ^ name
            ^ match t with Some t -> " " ^ show_type t | None -> "")
          variants ""
    | NewType inner -> "newtype " ^ show_type inner
    | Var { id } -> "var " ^ Id.show id
    | InferVar var -> (
        match (Inference.get_inferred var : value option) with
        | Some (Type (InferVar inner))
          when Inference.get_id inner = Inference.get_id var ->
            "<not inferred " ^ Inference.show_id var ^ ">"
        | Some (Type inferred) -> show_type inferred
        | Some _ -> failwith "type was inferred as not a type wtf"
        | None -> "<not inferred " ^ Inference.show_id var ^ ">")
    | MultiSet t -> "multiset of " ^ show_type t
    | MultiSetOldToRemove _ -> failwith "todo show multiset"
    | Builtin name -> "builtin " ^ name

  and ir_name : 'a. 'a ir_node -> string = function
    | Use _ -> "use"
    | Void _ -> "void"
    | Builtin _ -> "builtin"
    | Assign _ -> "assign"
    | ConstructVariant _ -> "construct_variant"
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
    | Tuple _ -> "tuple"
    | UnwindableBlock _ -> "unwindable_block"
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
    | MultiSet _ -> "multiset"

  and show_ir_with_inference_data :
      (pattern_data -> string option) ->
      ('a -> string option) ->
      'a ir_node ->
      string =
   fun show_pattern_data show_data ->
    let show_rec_pat : pattern -> string =
     fun p -> show_pattern_with_data show_pattern_data p
    in
    let rec show_rec : 'a ir_node -> string =
     fun ir ->
      let ir_itself =
        match ir with
        | Use { namespace; _ } -> "use " ^ show namespace
        | Void _ -> "void"
        | Struct { body; _ } -> "struct (" ^ show_rec body ^ ")"
        | Assign { pattern; value; _ } ->
            show_pattern pattern ^ " = " ^ show_rec value
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
        | ConstructVariant { ty; variant; value; _ } -> (
            show_or "" show_type ty ^ "." ^ variant
            ^ match value with Some value -> " " ^ show_rec value | None -> "")
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
        | NewType { def; _ } -> "newtype " ^ show_ir def
        | Scope { expr; _ } -> "(" ^ show_rec expr ^ ")"
        | TypeOf { expr; _ } -> "typeof " ^ show_rec expr
        | TypeOfValue { expr; _ } -> "typeofvalue " ^ show_rec expr
        | Template { f; _ } -> "template " ^ show_fn f
        | Function _ -> "function"
        | UnwindableBlock { f; _ } -> "unwindable_block " ^ show_rec f
        | WithContext { new_context; expr; _ } ->
            "with " ^ show_rec new_context ^ " (" ^ show_rec expr ^ ")"
        | CurrentContext { context_type; _ } ->
            "current_context " ^ show_type context_type
        | Tuple { unnamed_fields; named_fields; data = _ } ->
            "( "
            ^ List.fold_left
                (fun acc field ->
                  (if acc = "" then "" else acc ^ ", ") ^ show_rec field)
                "" unnamed_fields
            ^ StringMap.fold
                (fun name field acc ->
                  (if acc = "" then "" else acc ^ ", ")
                  ^ name ^ ": " ^ show_rec field)
                named_fields ""
            ^ " )"
        | Number { raw; _ } -> raw
        | Ast _ -> "ast"
        | Const { value; _ } -> "(const " ^ show value ^ ")"
        | FieldAccess { obj; name; _ } ->
            "(field " ^ show_rec obj ^ " " ^ name ^ ")"
        | Builtin { name; _ } -> "builtin " ^ String.escaped name
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
        | MultiSet { a; b; _ } ->
            "| " ^ show_ir a ^ show_or "" (fun b -> "| " ^ show_ir b) b
      in
      ir_itself ^ show_or "" (fun s -> s) (show_data (ir_data ir))
    in
    show_rec

  and show_ir : ir -> string =
   fun ir ->
    let show_inference_data (data : type_inference_data) =
      let result =
        match Inference.get_inferred data.type_var with
        | None -> None
        | Some inferred -> Some (" :: " ^ show inferred)
      in
      result
    in
    show_ir_with_inference_data show_inference_data
      (fun ir_data -> show_inference_data ir_data.inference)
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
      | Binding { binding; _ } ->
          "<" ^ binding.name ^ " " ^ Id.show binding.id ^ ">"
      | Variant { name; value; _ } ->
          name ^ show_or "" (fun value -> " " ^ show_rec value) value
      | Tuple { unnamed_fields; named_fields; _ } ->
          "( "
          ^ List.fold_left
              (fun acc field ->
                (if acc = "" then "" else acc ^ ", ") ^ show_rec field)
              "" unnamed_fields
          ^ StringMap.fold
              (fun name field acc ->
                (if acc = "" then "" else acc ^ ", ")
                ^ name ^ ": " ^ show_rec field)
              named_fields ""
          ^ " )"
    in
    show_rec pattern
end
