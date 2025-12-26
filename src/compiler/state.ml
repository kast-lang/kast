open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Error

module Scope = struct
  type scope = Types.compiler_scope
  type t = scope

  let init ~recursive : scope =
    {
      id = Id.gen ();
      parent = None;
      bindings = StringMap.empty;
      recursive;
      closed = false;
      on_update = [];
    }

  let enter ~recursive ~(parent : scope) : scope =
    {
      id = Id.gen ();
      parent = Some parent;
      bindings = StringMap.empty;
      recursive;
      closed = false;
      on_update = [];
    }

  type _ Effect.t += AwaitUpdate : scope -> bool Effect.t

  let rec find_binding_opt : from:span -> string -> scope -> binding option =
   fun ~from ident scope ->
    (* Log.info (fun log -> log "Looking for %S in %a" ident Id.print scope.id); *)
    match StringMap.find_opt ident scope.bindings with
    | Some binding ->
        Label.add_reference from binding.label;
        Some binding
    | None ->
        let find_in_parent () =
          scope.parent |> Option.and_then (find_binding_opt ~from ident)
        in
        if scope.recursive && not scope.closed then
          if
            (* Log.info (fun log ->
              log "Waiting for %S symbol in recursive scope %a" ident Id.print
                scope.id); *)
            Effect.perform (AwaitUpdate scope)
          then
            (* Log.info (fun log ->
                log "Waited for %S symbol in recursive scope %a" ident Id.print
                  scope.id); *)
            find_binding_opt ~from ident scope
          else find_in_parent ()
        else find_in_parent ()

  let fork (f : unit -> unit) : unit =
    Kast_interpreter.fork (fun () ->
        try f ()
        with effect AwaitUpdate scope, k ->
          (* println "registering waiter for %a" Id.print scope.id; *)
          let k = dont_leak_please k in
          scope.on_update <- (fun () -> k.continue true) :: scope.on_update)

  let notify_update (scope : scope) : unit =
    let fs = scope.on_update in
    scope.on_update <- [];
    (* println "# of waiters: %d" (List.length fs); *)
    fs |> List.iter (fun f -> f ())

  let find_binding :
      from_scope:VarScope.t -> from:span -> string -> scope -> binding =
   fun ~from_scope ~from ident scope ->
    scope
    |> find_binding_opt ~from ident
    |> Option.unwrap_or_else (fun () : binding ->
        error from "Could not find %S in scope" ident;
        {
          id = Id.gen ();
          scope = from_scope;
          name = Symbol.create ident;
          span = from;
          ty = Ty.new_not_inferred ~scope:from_scope ~span:from;
          label = Label.create_definition from ident;
          mut = false;
        })

  let close : scope -> unit =
   fun scope ->
    scope.closed <- true;
    (* println "closed %a" Id.print scope.id; *)
    notify_update scope

  let inject_binding : binding -> scope -> scope =
   fun binding scope ->
    if scope.recursive then (
      scope.bindings <-
        scope.bindings |> StringMap.add binding.name.name binding;
      (* println "injected %S" binding.name.name; *)
      notify_update scope;
      scope)
    else (
      close scope;
      {
        id = Id.gen ();
        parent = scope.parent;
        bindings = scope.bindings |> StringMap.add binding.name.name binding;
        recursive = false;
        closed = false;
        on_update = [];
      })
end

type imported = {
  custom_syntax_impls : (Id.t, value) Hashtbl.t;
  cast_impls : Types.cast_impls;
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
  mut_enabled : bool;
  by_ref : bool;
}

type state = t

let blank name_part ~import_cache =
  {
    scope = Scope.init ~recursive:false;
    currently_compiled_file = None;
    import_cache;
    interpreter = Interpreter.default name_part;
    custom_syntax_impls = Hashtbl.create 0;
    mut_enabled = false;
    by_ref = false;
  }

let enter_scope : span:span -> recursive:bool -> state -> state =
 fun ~span ~recursive
     {
       scope;
       currently_compiled_file;
       interpreter;
       import_cache;
       custom_syntax_impls;
       mut_enabled;
       by_ref;
     } ->
  {
    scope = Scope.enter ~recursive ~parent:scope;
    interpreter = Interpreter.enter_scope ~span ~recursive interpreter;
    currently_compiled_file;
    import_cache;
    custom_syntax_impls;
    mut_enabled;
    by_ref;
  }

let var_scope : t -> VarScope.t = fun state -> Some state.interpreter.scope
