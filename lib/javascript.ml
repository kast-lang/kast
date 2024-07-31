open Prelude
open Types

type var = { id : id }
type js = { code : string; var : var }

let var_name : var -> string = fun var -> "var" ^ Id.show var.id

let assign : var -> string -> string =
 fun var value -> var_name var ^ " = " ^ value ^ "; "

let rec compile : state -> ir -> js =
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
        let obj = compile self obj in
        assert (default_value |> Option.is_none);
        obj.code ^ assign var (var_name obj.var ^ "." ^ name)
    | Const _ -> failwith @@ "todo Const"
    | Binding { binding; data = _ } -> (
        match get_local_opt self binding.name with
        | Some local -> ()
        | None -> assign var (var_name { id = binding.id }))
    | Number _ -> failwith @@ "todo Number"
    | String { data = _; value; raw = _ } ->
        assign var ("\"" ^ String.escaped value ^ "\"")
    | Discard _ -> failwith @@ "todo Discard"
    | Then _ -> failwith @@ "todo Then"
    | Call { f; args; data = _ } ->
        let f = compile self f in
        let args = compile self args in
        f.code ^ args.code
        ^ assign var (var_name f.var ^ "(" ^ var_name args.var ^ ")")
    | Instantiate _ -> failwith @@ "todo Instantiate"
    | Builtin _ -> failwith @@ "todo Builtin"
    | BuiltinFn _ -> failwith @@ "todo BuiltinFn"
    | If _ -> failwith @@ "todo If"
    | Let _ -> failwith @@ "todo Let"
  in
  { code; var }
