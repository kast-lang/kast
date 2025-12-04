open Std
open Kast_util
open Kast_types

type scope = InterpreterScope.t
type t = scope

module Locals = struct
  type t = InterpreterScope.Locals.t

  let empty : t = { by_symbol = SymbolMap.empty }
end

type locals = Locals.t

let with_values ~recursive ~parent values : scope =
  {
    id = Id.gen ();
    locals = values;
    parent;
    recursive;
    on_update = [];
    closed = false;
  }

let init ~recursive ~parent = with_values ~recursive ~parent Locals.empty

type _ Effect.t += AwaitUpdate : (symbol * scope) -> bool Effect.t

let rec find_local_opt (name : symbol) (scope : scope) :
    Types.interpreter_local option =
  match SymbolMap.find_opt name scope.locals.by_symbol with
  | Some value -> Some value
  | None ->
      let find_in_parent () =
        scope.parent |> Option.and_then (find_local_opt name)
      in
      if scope.recursive && not scope.closed then (
        Log.trace (fun log ->
            log "awaiting scope update for %a" Symbol.print name);
        let await_result = Effect.perform (AwaitUpdate (name, scope)) in
        Log.trace (fun log ->
            log "awaited scope update for %a" Symbol.print name);
        if await_result then find_local_opt name scope else find_in_parent ())
      else find_in_parent ()

let find_opt (name : symbol) (scope : scope) : place option =
  find_local_opt name scope
  |> Option.map (fun (local : Types.interpreter_local) -> local.place)

let notify_update (scope : scope) : unit =
  let fs = scope.on_update in
  scope.on_update <- [];
  fs |> List.iter (fun f -> f ())

(* TODO should be removed? *)
let add_locals (new_locals : locals) (scope : scope) : unit =
  Log.trace (fun log ->
      new_locals.by_symbol
      |> SymbolMap.iter (fun symbol (local : Types.interpreter_local) ->
          log "Added %a=%a in scope %a" Symbol.print symbol Place.print_value
            local.place Id.print scope.id));
  scope.locals <-
    {
      by_symbol =
        SymbolMap.union
          (fun _name _old_value new_value -> Some new_value)
          scope.locals.by_symbol new_locals.by_symbol;
    };
  notify_update scope

let add_local_existing_place (span : span) (symbol : symbol) (place : place)
    (scope : scope) : unit =
  scope
  |> add_locals
       {
         by_symbol =
           SymbolMap.singleton symbol
             ({
                place;
                ty_field =
                  {
                    ty = place.ty;
                    label = Label.create_reference span symbol.name;
                  };
              }
               : Types.interpreter_local);
       }

let add_local (span : span) (symbol : symbol) (value : value) (scope : scope) :
    unit =
  Log.trace (fun log ->
      log "Added %a=%a in scope %a" Symbol.print symbol Value.print value
        Id.print scope.id);
  scope.locals <-
    {
      by_symbol =
        scope.locals.by_symbol
        |> SymbolMap.add symbol
             ({
                place = Place.init value;
                ty_field =
                  {
                    ty = Value.ty_of value;
                    label = Label.create_definition span symbol.name;
                  };
              }
               : Types.interpreter_local);
    };
  notify_update scope

let inject_binding (binding : binding) (scope : scope) : unit =
  scope
  |> add_local binding.span binding.name
       (V_Binding binding |> Value.inferred ~span:binding.span)

let rec print_all : formatter -> scope -> unit =
 fun fmt scope ->
  fprintf fmt "(rec=%b,closed=%b)" scope.recursive scope.closed;
  scope.locals.by_symbol
  |> SymbolMap.iter (fun symbol _value -> fprintf fmt "%a," Symbol.print symbol);
  match scope.parent with
  | None -> ()
  | Some parent ->
      fprintf fmt "^";
      print_all fmt parent

let close : scope -> unit =
 fun scope ->
  scope.closed <- true;
  (* println "closed %a" print_all scope; *)
  notify_update scope
