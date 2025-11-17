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

let with_values ~parent values : scope = { locals = values; parent }
let init ~parent = with_values ~parent Locals.empty

let rec assign_to_existing ~(span : span) (name : symbol) (value : value)
    (scope : scope) : unit =
  match SymbolMap.find_opt name scope.locals.by_symbol with
  | Some existing -> existing.value <- value
  | None -> (
      match scope.parent with
      | Some parent -> parent |> assign_to_existing ~span name value
      | None ->
          Error.error span "Trying to assign to non-existing %a" Symbol.print
            name;
          scope.locals <-
            {
              by_symbol =
                scope.locals.by_symbol
                |> SymbolMap.add name
                     ({
                        value;
                        ty_field =
                          {
                            ty = Value.ty_of value;
                            span (* TODO maybe should add name's span *);
                            references = [];
                          };
                      }
                       : Types.interpreter_local);
            })

let rec find_local_opt (name : symbol) (scope : scope) :
    Types.interpreter_local option =
  match SymbolMap.find_opt name scope.locals.by_symbol with
  | Some value -> Some value
  | None -> (
      match scope.parent with
      | Some parent -> find_local_opt name parent
      | None -> None)

let find_opt (name : symbol) (scope : scope) : value option =
  find_local_opt name scope
  |> Option.map (fun (local : Types.interpreter_local) -> local.value)

let add_locals (new_locals : locals) (scope : scope) : unit =
  scope.locals <-
    {
      by_symbol =
        SymbolMap.union
          (fun _name _old_value new_value -> Some new_value)
          scope.locals.by_symbol new_locals.by_symbol;
    }

let add_local (span : span) (symbol : symbol) (value : value) (scope : scope) :
    unit =
  scope.locals <-
    {
      by_symbol =
        scope.locals.by_symbol
        |> SymbolMap.add symbol
             ({
                value;
                ty_field = { ty = Value.ty_of value; span; references = [] };
              }
               : Types.interpreter_local);
    }

let rec print_all : formatter -> scope -> unit =
 fun fmt scope ->
  scope.locals.by_symbol
  |> SymbolMap.iter (fun symbol _value -> fprintf fmt "%a," Symbol.print symbol);
  match scope.parent with
  | None -> ()
  | Some parent ->
      fprintf fmt "^";
      print_all fmt parent
