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

let rec assign_to_existing (name : symbol) (value : value) (scope : scope) :
    unit =
  match SymbolMap.find_opt name scope.locals.by_symbol with
  | Some _ ->
      scope.locals <-
        { by_symbol = scope.locals.by_symbol |> SymbolMap.add name value }
  | None -> (
      match scope.parent with
      | Some parent -> parent |> assign_to_existing name value
      | None -> fail "Trying to assign to non-existing %a" Symbol.print name)

let rec find_opt (name : symbol) (scope : scope) : value option =
  match SymbolMap.find_opt name scope.locals.by_symbol with
  | Some value -> Some value
  | None -> (
      match scope.parent with
      | Some parent -> find_opt name parent
      | None -> None)

let add_locals (new_locals : locals) (scope : scope) : unit =
  scope.locals <-
    {
      by_symbol =
        SymbolMap.union
          (fun _name _old_value new_value -> Some new_value)
          scope.locals.by_symbol new_locals.by_symbol;
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
