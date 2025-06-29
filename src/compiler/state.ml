open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast

module Scope = struct
  type scope = {
    parent : scope option;
    bindings : binding StringMap.t;
  }

  type t = scope

  let init () : scope = { parent = None; bindings = StringMap.empty }

  let enter ~(parent : scope) : scope =
    { parent = Some parent; bindings = StringMap.empty }

  let rec find_binding_opt : Token.Shape.ident -> scope -> binding option =
   fun ident scope ->
    match StringMap.find_opt ident.name scope.bindings with
    | Some binding -> Some binding
    | None -> scope.parent |> Option.and_then (find_binding_opt ident)

  let find_binding : Token.Shape.ident -> scope -> binding =
   fun ident scope ->
    scope |> find_binding_opt ident
    |> Option.unwrap_or_else (fun () : binding ->
           (* TODO fail "Could not find %S in scope" ident.name *)
           { name = ident.name; ty = Ty.new_not_inferred () })

  let inject_binding : binding -> scope -> scope =
   fun binding { parent; bindings } ->
    { parent; bindings = bindings |> StringMap.add binding.name binding }
end

type t = {
  (* TODO do this properly *)
  mutable scope : Scope.t;
  interpreter : Interpreter.state;
}

type state = t

let enter_scope : state -> state =
 fun { scope; interpreter } ->
  { scope = Scope.enter ~parent:scope; interpreter }
