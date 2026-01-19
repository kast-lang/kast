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
  type local = Types.compiler_local

  module Local = struct
    let binding (local : local) : binding =
      match local with
      | Const { binding; place = _ } | Binding binding -> binding
    ;;
  end

  let init ~recursive : scope =
    { id = Id.gen ()
    ; parent = None
    ; locals = StringMap.empty
    ; recursive
    ; closed = false
    ; on_update = []
    }
  ;;

  let enter ~recursive (parent : scope) : scope =
    let id = Id.gen () in
    Log.trace (fun log -> log "Entered scope %a -> %a" Id.print parent.id Id.print id);
    { id
    ; parent = Some parent
    ; locals = StringMap.empty
    ; recursive
    ; closed = false
    ; on_update = []
    }
  ;;

  type _ Effect.t += AwaitUpdate : scope -> bool Effect.t

  let rec find_opt : from:span -> string -> scope -> local option =
    fun ~from ident scope ->
    (* Log.trace (fun log -> log "Looking for %S in %a" ident Id.print scope.id); *)
    match StringMap.find_opt ident scope.locals with
    | Some local ->
      let binding = Local.binding local in
      Label.add_reference from binding.label;
      Some local
    | None ->
      let find_in_parent () = scope.parent |> Option.and_then (find_opt ~from ident) in
      if scope.recursive && not scope.closed
      then
        if
          (* Log.trace (fun log ->
              log "Waiting for %S symbol in recursive scope %a" ident Id.print
                scope.id); *)
          Effect.perform (AwaitUpdate scope)
        then
          (* Log.trace (fun log ->
                log "Waited for %S symbol in recursive scope %a" ident Id.print
                  scope.id); *)
          find_opt ~from ident scope
        else find_in_parent ()
      else find_in_parent ()
  ;;

  let fork (f : unit -> unit) : unit =
    Kast_interpreter.fork (fun () ->
      try f () with
      | effect AwaitUpdate scope, k ->
        let k = dont_leak_please k in
        scope.on_update <- (fun () -> k.continue true) :: scope.on_update)
  ;;

  let notify_update (scope : scope) : unit =
    let fs = scope.on_update in
    scope.on_update <- [];
    fs |> List.iter (fun f -> f ())
  ;;

  let find : from_scope:VarScope.t -> from:span -> string -> scope -> local =
    fun ~from_scope ~from ident scope ->
    scope
    |> find_opt ~from ident
    |> Option.unwrap_or_else (fun () : local ->
      error from "Could not find %S in scope" ident;
      Binding
        { id = Id.gen ()
        ; scope = from_scope
        ; name = Symbol.create ident
        ; span = from
        ; ty = Ty.new_not_inferred ~scope:from_scope ~span:from
        ; label = Label.create_definition from ident
        ; mut = false
        ; hygiene = CallSite
        ; def_site = None
        })
  ;;

  let close : scope -> unit =
    fun scope ->
    scope.closed <- true;
    notify_update scope
  ;;

  let add : local -> scope -> scope =
    fun local scope ->
    let name = (Local.binding local).name.name in
    if scope.recursive
    then (
      scope.locals <- scope.locals |> StringMap.add name local;
      notify_update scope;
      scope)
    else (
      close scope;
      let id = Id.gen () in
      Log.trace (fun log ->
        log
          "Injected %S into %a (parent=%a) (original scope=%a)"
          name
          Id.print
          id
          (Option.print Id.print)
          (scope.parent |> Option.map (fun (scope : scope) -> scope.id))
          Id.print
          scope.id);
      { id
      ; parent = scope.parent
      ; locals = scope.locals |> StringMap.add name local
      ; recursive = false
      ; closed = false
      ; on_update = []
      })
  ;;
end

module Scopes = struct
  type t =
    { call_site : Scope.t
    ; def_sites : Scope.t Id.Map.t
    ; def_site : Scope.t option
    }

  let only scope = { call_site = scope; def_sites = Id.Map.empty; def_site = None }
  let init ~recursive = only (Scope.init ~recursive)

  let map f scopes =
    { call_site = scopes.call_site |> f
    ; def_sites = scopes.def_sites |> Id.Map.map f
    ; def_site = scopes.def_site
    }
  ;;

  let enter ~recursive = map (Scope.enter ~recursive)

  let enter_def_site ~span (def_site : Scope.t) scopes =
    { call_site = scopes.call_site
    ; def_sites =
        scopes.def_sites
        |> Id.Map.update def_site.id (fun current ->
          Some
            (match current with
             | Some (result : Scope.t) ->
               Log.trace (fun log ->
                 log
                   "Entered (already) def site %a = %a at %a"
                   Id.print
                   def_site.id
                   Id.print
                   result.id
                   Span.print
                   span);
               result
             | None ->
               let result = def_site |> Scope.enter ~recursive:false in
               Log.trace (fun log ->
                 log
                   "Entered def site %a = %a at %a"
                   Id.print
                   def_site.id
                   Id.print
                   result.id
                   Span.print
                   span);
               result))
    ; def_site = Some def_site
    }
  ;;

  let add (local : Scope.local) scopes =
    let old_def_site = scopes.def_site in
    let binding = Scope.Local.binding local in
    let def_site =
      match binding.hygiene with
      | CallSite -> None
      | DefSite -> binding.def_site
    in
    let scopes =
      match def_site with
      | Some def_site ->
        Log.trace (fun log ->
          log
            "injecting into def site for %a = %a at %a"
            Binding.print
            binding
            Id.print
            def_site.id
            Span.print
            binding.span);
        scopes |> enter_def_site ~span:binding.span def_site
      | None -> scopes
    in
    match def_site with
    | None ->
      { call_site = scopes.call_site |> Scope.add local
      ; def_sites = scopes.def_sites
      ; def_site = old_def_site
      }
    | Some def_site ->
      { call_site = scopes.call_site
      ; def_sites =
          scopes.def_sites
          |> Id.Map.update def_site.id (fun scope ->
            Some (scope |> Option.get |> Scope.add local))
      ; def_site = old_def_site
      }
  ;;

  let close scopes =
    scopes
    |> map (fun scope ->
      Scope.close scope;
      scope)
    |> ignore
  ;;

  let bindings scopes =
    scopes.call_site.locals |> StringMap.map Scope.Local.binding |> StringMap.to_list
  ;;

  (* TODO maybe def sites too? *)

  let call_site scopes = scopes.call_site

  let def_site scopes =
    match scopes.def_site with
    | Some def_site -> scopes.def_sites |> Id.Map.find def_site.id
    | None -> scopes.call_site
  ;;

  let find ~(hygiene : Types.ast_hygiene) ~from_scope ~from label scopes =
    let scope =
      match hygiene with
      | CallSite -> scopes.call_site
      | DefSite ->
        Log.trace (fun log ->
          log
            "Finding %S in def site %a"
            label
            (Option.print Id.print)
            (scopes.def_site |> Option.map (fun (scope : Scope.t) -> scope.id)));
        def_site scopes
    in
    Log.trace (fun log -> log "Finding %S in scope %a" label Id.print scope.id);
    scope |> Scope.find ~from_scope ~from label
  ;;
end

type imported =
  { custom_syntax_impls : (Id.t, value) Hashtbl.t
  ; cast_impls : Types.cast_impls
  ; value : value
  }

type import =
  | InProgress
  | Imported of imported

type import_cache = { mutable by_uri : import UriMap.t }

let init_import_cache () : import_cache = { by_uri = UriMap.empty }

type t =
  { (* TODO do this properly *)
    mutable scopes : Scopes.t
  ; mutable currently_compiled_file : Uri.t option
  ; import_cache : import_cache
  ; interpreter : Interpreter.state
  ; custom_syntax_impls : (Id.t, value) Hashtbl.t
  ; mut_enabled : bool
  ; bind_mode : Types.bind_mode
  }

type state = t

let blank name_part ~import_cache =
  { scopes = Scopes.init ~recursive:false
  ; currently_compiled_file = None
  ; import_cache
  ; interpreter = Interpreter.default name_part
  ; custom_syntax_impls = Hashtbl.create 0
  ; mut_enabled = false
  ; bind_mode = Claim
  }
;;

let enter_scope : span:span -> recursive:bool -> state -> state =
  fun ~span
    ~recursive
    { scopes
    ; currently_compiled_file
    ; interpreter
    ; import_cache
    ; custom_syntax_impls
    ; mut_enabled
    ; bind_mode
    } ->
  { scopes = scopes |> Scopes.enter ~recursive
  ; interpreter =
      Interpreter.enter_scope ~new_result_scope:true ~span ~recursive interpreter
  ; currently_compiled_file
  ; import_cache
  ; custom_syntax_impls
  ; mut_enabled
  ; bind_mode
  }
;;

let var_scope : t -> VarScope.t = fun state -> state.interpreter.result_scope
