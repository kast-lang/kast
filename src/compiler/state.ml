open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Error

module Scope = struct
  type scope = {
    parent : scope option;
    bindings : binding StringMap.t;
  }

  type t = scope

  let init () : scope = { parent = None; bindings = StringMap.empty }

  let enter ~(parent : scope) : scope =
    { parent = Some parent; bindings = StringMap.empty }

  let rec find_binding_opt : from:span -> string -> scope -> binding option =
   fun ~from ident scope ->
    match StringMap.find_opt ident scope.bindings with
    | Some binding ->
        binding.references <- from :: binding.references;
        Some binding
    | None -> scope.parent |> Option.and_then (find_binding_opt ~from ident)

  let find_binding : from:span -> string -> scope -> binding =
   fun ~from ident scope ->
    scope
    |> find_binding_opt ~from ident
    |> Option.unwrap_or_else (fun () : binding ->
           error from "Could not find %S in scope" ident;
           {
             name = Symbol.create ident;
             span = from;
             ty = Ty.new_not_inferred ();
             references = [];
           })

  let inject_binding : binding -> scope -> scope =
   fun binding { parent; bindings } ->
    { parent; bindings = bindings |> StringMap.add binding.name.name binding }
end

type imported = {
  custom_syntax_impls : (Id.t, value) Hashtbl.t;
  value : value;
}

type import =
  | InProgress
  | Imported of imported

type import_cache = { mutable by_uri : import UriMap.t }

let init_import_cache () : import_cache = { by_uri = UriMap.empty }

type t = {
  (* TODO do this properly *)
  mutable scope : Scope.t;
  mutable currently_compiled_file : Uri.t option;
  import_cache : import_cache;
  interpreter : Interpreter.state;
  custom_syntax_impls : (Id.t, value) Hashtbl.t;
}

type state = t

let blank ~import_cache =
  {
    scope = Scope.init ();
    currently_compiled_file = None;
    import_cache;
    interpreter = Interpreter.default ();
    custom_syntax_impls = Hashtbl.create 0;
  }

let enter_scope : state -> state =
 fun {
       scope;
       currently_compiled_file;
       interpreter;
       import_cache;
       custom_syntax_impls;
     } ->
  {
    scope = Scope.enter ~parent:scope;
    currently_compiled_file;
    interpreter;
    import_cache;
    custom_syntax_impls;
  }
