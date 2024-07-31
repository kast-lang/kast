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

  let var_name : var -> string = fun var -> "var" ^ Id.show var.id

  let assign : var -> string -> string =
   fun var value -> var_name var ^ " = " ^ value ^ "; "

  let rec compile_ir : state -> ir -> js =
   fun self ir ->
    let var = { id = Id.gen () } in
    let void () = assign var "undefined" in
    let code =
      match ir with
      | Void _ -> void ()
      | Use _ -> failwith @@ "todo Use"
      | Struct _ -> failwith @@ "todo Struct"
      | Assign _ -> failwith @@ "todo Assign"
      | CreateImpl _ -> failwith @@ "todo CreateImpl"
      | GetImpl _ -> failwith @@ "todo GetImpl"
      | CheckImpl _ -> failwith @@ "todo CheckImpl"
      | Match _ -> failwith @@ "todo Match"
      | NewType _ -> failwith @@ "todo NewType"
      | Scope _ -> failwith @@ "todo Scope"
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
      | FieldAccess { obj; name; default_value; data = _ } ->
          let obj = compile_ir self obj in
          assert (default_value |> Option.is_none);
          obj.code ^ assign var (var_name obj.var ^ "." ^ name)
      | Const _ -> failwith @@ "todo Const"
      | Binding { binding; data = _ } -> (
          match Interpreter.get_local_opt self binding.name with
          | Some { value = Binding _; _ } | None ->
              assign var (var_name { id = binding.id })
          | Some { value; binding } ->
              (* TODO compile multiple references to same thing only once *)
              assign var @@ compile_value value)
      | Number _ -> failwith @@ "todo Number"
      | String { data = _; value; raw = _ } ->
          assign var ("\"" ^ String.escaped value ^ "\"")
      | Discard _ -> failwith @@ "todo Discard"
      | Then _ -> failwith @@ "todo Then"
      | Call { f; args; data = _ } ->
          let f = compile_ir self f in
          let args = compile_ir self args in
          f.code ^ args.code
          ^ assign var (var_name f.var ^ "(" ^ var_name args.var ^ ")")
      | Instantiate _ -> failwith @@ "todo Instantiate"
      | Builtin _ -> failwith @@ "todo Builtin"
      | BuiltinFn _ -> failwith @@ "todo BuiltinFn"
      | If _ -> failwith @@ "todo If"
      | Let _ -> failwith @@ "todo Let"
    in
    { code; var }

  and compile_value : value -> string = function
    | Binding _ -> failwith @@ "Binding cant be compiled into js"
    | Var _ -> failwith @@ "Var cant be compiled into js"
    | InferVar _ -> failwith @@ "InferVar cant be compiled into js"
    | UnwindToken _ -> failwith @@ "UnwindToken cant be compiled into js"
    | DelimitedToken _ -> failwith @@ "DelimitedToken cant be compiled into js"
    | Ast _ -> failwith @@ "Ast cant be compiled into js"
    | Macro _ -> failwith @@ "Macro cant be compiled into js"
    | BuiltinMacro _ -> failwith @@ "BuiltinMacro cant be compiled into js"
    | BuiltinFn { f; ty } -> (
        match f.name with
        | "print" -> "console.log"
        | _ -> failwith @@ "builtin fn " ^ f.name ^ " is not implemented for js"
        )
    | Template _ -> failwith @@ "Template cant be compiled into js"
    | Function f ->
        let f = Compiler.ensure_compiled f in
        let body = compile_ir f.captured f.body in
        "function () { " ^ body.code ^ "return " ^ var_name body.var ^ "; }"
    | Void -> failwith @@ "Void cant be compiled into js"
    | Bool _ -> failwith @@ "Bool cant be compiled into js"
    | Int32 _ -> failwith @@ "Int32 cant be compiled into js"
    | Int64 _ -> failwith @@ "Int64 cant be compiled into js"
    | Float64 _ -> failwith @@ "Float64 cant be compiled into js"
    | String _ -> failwith @@ "String cant be compiled into js"
    | Dict _ -> failwith @@ "Dict cant be compiled into js"
    | Struct _ -> failwith @@ "Struct cant be compiled into js"
    | Ref _ -> failwith @@ "Ref cant be compiled into js"
    | Type _ -> failwith @@ "Type cant be compiled into js"
    | Variant _ -> failwith @@ "Variant cant be compiled into js"
end
