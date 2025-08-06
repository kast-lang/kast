open Std
open Kast_util

module Scope = struct
  module Locals = struct
    type t = { by_symbol : Value.t SymbolMap.t }

    let empty : t = { by_symbol = SymbolMap.empty }
  end

  type t = {
    mutable locals : Locals.t;
    parent : t option;
  }

  type scope = t

  let with_values ~parent values : scope = { locals = values; parent }
  let init ~parent = with_values ~parent Locals.empty

  let rec assign_to_existing ~(span : span) (name : symbol) (value : Value.t)
      (scope : scope) : unit =
    match SymbolMap.find_opt name scope.locals.by_symbol with
    | Some _ ->
        scope.locals <-
          { by_symbol = scope.locals.by_symbol |> SymbolMap.add name value }
    | None -> (
        match scope.parent with
        | Some parent -> parent |> assign_to_existing ~span name value
        | None ->
            Error.throw span "Trying to assign to non-existing %a" Symbol.print
              name;
            scope.locals <-
              { by_symbol = scope.locals.by_symbol |> SymbolMap.add name value }
        )

  let rec find_opt (name : symbol) (scope : scope) : Value.t option =
    match SymbolMap.find_opt name scope.locals.by_symbol with
    | Some value -> Some value
    | None -> (
        match scope.parent with
        | Some parent -> find_opt name parent
        | None -> None)

  let add_locals (new_locals : Locals.t) (scope : scope) : unit =
    scope.locals <-
      {
        by_symbol =
          SymbolMap.union
            (fun _name _old_value new_value -> Some new_value)
            scope.locals.by_symbol new_locals.by_symbol;
      }

  let add_local (symbol : symbol) (value : Value.t) (scope : scope) : unit =
    scope.locals <-
      { by_symbol = scope.locals.by_symbol |> SymbolMap.add symbol value }

  let rec print_all : formatter -> scope -> unit =
   fun fmt scope ->
    scope.locals.by_symbol
    |> SymbolMap.iter (fun symbol _value ->
           fprintf fmt "%a," Symbol.print symbol);
    match scope.parent with
    | None -> ()
    | Some parent ->
        fprintf fmt "^";
        print_all fmt parent
end

type t = { scope : Scope.t }

let default () : t = { scope = Scope.init ~parent:None }

module AbstractEval = struct
  module type E = sig
    include Shaped.S
    module Result : Shaped.S

    val shape : t -> Shape.t
  end

  module Make (E : E) = struct
    type eval_fn = E.t -> (t -> E.Result.t) option

    let eval_impls : eval_fn list Atomic.t = Atomic.make []

    let register : eval_fn -> unit =
     fun f -> Atomic.set eval_impls (f :: Atomic.get eval_impls)

    let eval : E.t -> t -> E.Result.t =
     fun expr state ->
      match E.shape expr with
      | E.Shape.Error.T -> E.Result.error ()
      | _ ->
          let f =
            Atomic.get eval_impls
            |> List.find_map (fun f -> f expr)
            |> Option.unwrap_or_else (fun () -> failwith __LOC__)
          in
          f state
  end
end

module EvalExpr = AbstractEval.Make (Expr)
module EvalTy = AbstractEval.Make (Expr.Ty)

module Eval = struct
  module Expr = EvalExpr

  let expr = Expr.eval

  module Ty = EvalTy

  let ty = Ty.eval
end

module PatternMatch = struct
  type matches = Value.t SymbolMap.t
  type match_fn = Pattern.t -> (Value.t -> matches) option

  let match_impls : match_fn list Atomic.t = Atomic.make []

  let register : match_fn -> unit =
   fun f -> Atomic.set match_impls (f :: Atomic.get match_impls)

  let init () =
    register (fun pattern ->
        match pattern.shape with
        | Pattern.Shape.Unit -> Some (fun _ -> SymbolMap.empty)
        | Pattern.Shape.Error -> Some (fun _ -> SymbolMap.empty)
        | Pattern.Shape.Binding binding ->
            Some (fun value -> SymbolMap.singleton binding.name value)
        | _ -> None)

  let perform : Pattern.t -> Value.t -> matches =
   fun pattern value ->
    let f =
      Atomic.get match_impls
      |> List.find_map (fun f -> f pattern)
      |> Option.unwrap_or_else (fun () -> failwith __LOC__)
    in
    f value
end

let pattern_match = PatternMatch.perform
let init () = PatternMatch.init ()
