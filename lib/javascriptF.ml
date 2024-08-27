open Prelude
open Types

module Make
    (Show : Modules.Show)
    (Interpreter : Modules.Interpreter)
    (Compiler : Modules.Compiler)
    (Utils : Modules.Utils) =
struct
  type var = { id : id }
  type js = { code : string; var : var }
  type compiler_state = { mutable compiled_bindings : value Id.Map.t }

  let init_compiler : unit -> compiler_state =
   fun () -> { compiled_bindings = Id.Map.empty }

  let var_name : var -> string = fun var -> "var" ^ Id.show var.id

  let assign : var -> string -> string =
   fun var value -> var_name var ^ " = " ^ value ^ "; "

  let get_binding_value data binding =
    match Interpreter.get_local_opt data.state binding.name with
    | Some { value = Binding _; _ } | None -> None
    | Some { value; binding } -> Some value

  let assign_to_pattern (pattern : pattern) (var : var) : string =
    match pattern with
    | Placeholder _ -> ""
    | Void _ -> ""
    | Binding { binding; data = _ } -> assign { id = binding.id } (var_name var)
    | Dict _ -> failwith @@ "todo assign to dict"
    | Variant _ -> failwith @@ "todo assign to variant"
    | Union _ -> failwith @@ "todo assign to union"

  let rec compile_ir : compiler_state -> ir -> js =
   fun self ir ->
    let var = { id = Id.gen () } in
    let void () = assign var "undefined" in
    let code =
      match ir with
      | Void _ -> void ()
      | Use _ -> void ()
      | Struct _ -> failwith @@ "todo Struct"
      | Assign _ -> failwith @@ "todo Assign"
      | CreateImpl _ -> failwith @@ "todo CreateImpl"
      | GetImpl _ -> failwith @@ "todo GetImpl"
      | CheckImpl _ -> failwith @@ "todo CheckImpl"
      | Match _ -> failwith @@ "todo Match"
      | NewType _ -> failwith @@ "todo NewType"
      | Scope { expr; data = _ } ->
          let expr = compile_ir self expr in
          expr.code ^ assign var (var_name expr.var)
      | ConstructVariant _ -> failwith @@ "todo ConstructVariant"
      | OneOf _ -> failwith @@ "todo OneOf"
      | TypeOf _ -> failwith @@ "todo TypeOf"
      | TypeOfValue _ -> failwith @@ "todo TypeOfValue"
      | Dict _ -> failwith @@ "todo Dict"
      | UnwindableBlock _ -> failwith @@ "todo UnwindableBlock"
      | WithContext _ -> failwith @@ "todo WithContext"
      | CurrentContext _ -> failwith @@ "todo CurrentContext"
      | Ast _ -> failwith @@ "todo Ast"
      | Template _ -> failwith @@ "todo Template"
      | Function _ -> failwith @@ "todo Function"
      | FieldAccess { obj; name; default_value; data = _ } -> (
          let obj_value =
            match obj with
            | Binding { binding; data } -> get_binding_value data binding
            | _ -> None
          in
          match obj_value with
          | Some value -> (
              match Interpreter.get_field_opt value name with
              | Some value ->
                  let value = compile_value value in
                  value.code ^ assign var (var_name value.var)
              | None ->
                  let default = Option.get default_value in
                  let value = compile_ir self default in
                  value.code ^ assign var (var_name value.var))
          | None ->
              let obj = compile_ir self obj in
              assert (default_value |> Option.is_none);
              obj.code ^ assign var (var_name obj.var ^ "." ^ name))
      | Const _ -> failwith @@ "todo Const"
      | Binding { binding; data } ->
          (match get_binding_value data binding with
          | None -> ()
          | Some value ->
              if
                Id.Map.find_opt binding.id self.compiled_bindings
                |> Option.is_none
              then (
                Log.trace @@ "used " ^ binding.name;
                self.compiled_bindings <-
                  Id.Map.add binding.id value self.compiled_bindings));
          assign var (var_name { id = binding.id })
      | Number _ -> failwith @@ "todo Number"
      | String { data = _; value; raw = _ } ->
          assign var ("\"" ^ String.escaped value ^ "\"")
      | Discard _ -> failwith @@ "todo Discard"
      | Then { first; second; data = _ } ->
          let first = compile_ir self first in
          let second = compile_ir self second in
          first.code ^ second.code ^ assign var (var_name second.var)
      | Call { f; args; data = _ } ->
          let f = compile_ir self f in
          let args = compile_ir self args in
          f.code ^ args.code
          ^ assign var (var_name f.var ^ "(" ^ var_name args.var ^ ")")
      | Instantiate _ -> failwith @@ "todo Instantiate"
      | Builtin _ -> failwith @@ "todo Builtin"
      | BuiltinFn _ -> failwith @@ "todo BuiltinFn"
      | If _ -> failwith @@ "todo If"
      | Let { pattern; value; data = _ } ->
          let value = compile_ir self value in
          value.code ^ assign_to_pattern pattern value.var ^ void ()
      | MultiSet _ -> failwith @@ "todo MultiSet"
    in
    { code; var }

  and compile_value_with : compiler_state -> value -> js =
   fun self value ->
    let var = { id = Id.gen () } in
    let code =
      match value with
      | Binding _ -> failwith @@ "Binding cant be compiled into js"
      | Var _ -> failwith @@ "Var cant be compiled into js"
      | InferVar _ -> failwith @@ "InferVar cant be compiled into js"
      | UnwindToken _ -> failwith @@ "UnwindToken cant be compiled into js"
      | DelimitedToken _ ->
          failwith @@ "DelimitedToken cant be compiled into js"
      | Ast _ -> failwith @@ "Ast cant be compiled into js"
      | Ir ir ->
          let compiled = compile_ir self ir in
          compiled.code ^ assign var (var_name compiled.var)
      | Macro _ -> failwith @@ "Macro cant be compiled into js"
      | BuiltinMacro _ -> failwith @@ "BuiltinMacro cant be compiled into js"
      | Builtin { name; _ } | BuiltinFn { f = { name; _ }; _ } ->
          assign var (match name with "print" -> "console.log" | _ -> name)
      | Template _ -> failwith @@ "Template cant be compiled into js"
      | Function f ->
          (* TODO function arg *)
          let f = Compiler.ensure_compiled f in
          let body = compile_ir self f.body in
          assign var @@ "function () { " ^ body.code ^ "return "
          ^ var_name body.var ^ "; }"
      | Void -> failwith @@ "Void cant be compiled into js"
      | Bool _ -> failwith @@ "Bool cant be compiled into js"
      | Int32 _ -> failwith @@ "Int32 cant be compiled into js"
      | Int64 _ -> failwith @@ "Int64 cant be compiled into js"
      | Float64 _ -> failwith @@ "Float64 cant be compiled into js"
      | String _ -> failwith @@ "String cant be compiled into js"
      | Dict _ -> failwith @@ "Dict cant be compiled into js"
      | Struct s -> failwith @@ "Struct cant be compiled into js"
      | Ref _ -> failwith @@ "Ref cant be compiled into js"
      | Type _ -> failwith @@ "Type cant be compiled into js"
      | Variant _ -> failwith @@ "Variant cant be compiled into js"
      | MultiSet _ -> failwith @@ "MultiSet cant be compiled into js"
    in
    { code; var }

  and init_bindings (compiler : compiler_state) : string =
    let already_compiled = ref Id.Map.empty in
    let rec step () =
      let stepped = ref false in
      Id.Map.iter
        (fun id value ->
          if Id.Map.find_opt id !already_compiled |> Option.is_none then
            stepped := true;
          let this = compile_value_with compiler value in
          already_compiled := Id.Map.add id this !already_compiled)
        compiler.compiled_bindings;
      if !stepped then step ()
    in
    step ();
    Id.Map.fold
      (fun id js acc ->
        acc ^ "; " ^ js.code ^ "; " ^ assign { id } (var_name js.var))
      !already_compiled ""

  and with_compiler : (compiler_state -> js) -> js =
   fun f ->
    let compiler = init_compiler () in
    let js = f compiler in
    { code = init_bindings compiler ^ js.code; var = js.var }

  and compile_value : value -> js =
   fun value ->
    with_compiler (fun compiler -> compile_value_with compiler value)
end
