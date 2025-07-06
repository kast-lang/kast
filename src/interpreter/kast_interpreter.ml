open Std
open Kast_util
open Kast_types

type state = {
  natives : Natives.t;
  scope : Scope.t;
}

let init : Scope.locals -> state =
 fun values ->
  { scope = Scope.with_values ~parent:None values; natives = Natives.natives }

let default () = init Scope.Locals.empty

let pattern_match : value -> pattern -> Scope.locals =
 fun value pattern ->
  match pattern.shape with
  | P_Placeholder -> Scope.Locals.empty
  | P_Unit ->
      (* TODO assert that value is unit *)
      Scope.Locals.empty
  | P_Binding binding -> { by_symbol = SymbolMap.singleton binding.name value }

let assign : state -> Expr.assignee -> value -> unit =
 fun state assignee value ->
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit ->
      (* TODO assert that value is unit *)
      ()
  | A_Binding { name; ty = _; span = _; references = _ } ->
      state.scope |> Scope.assign_to_existing name value
  | A_Let pattern ->
      let new_bindings = pattern_match value pattern in
      state.scope |> Scope.add_locals new_bindings

let rec eval : state -> expr -> value =
 fun state expr ->
  match expr.shape with
  | E_Constant value -> value
  | E_Binding binding ->
      Scope.find_opt binding.name state.scope
      |> Option.unwrap_or_else (fun () ->
             Log.error "all in scope: %a" Scope.print_all state.scope;
             fail "%a not found" Symbol.print binding.name)
  | E_Fn def -> { shape = V_Fn { def; captured = state.scope } }
  | E_Tuple { tuple } ->
      { shape = V_Tuple { tuple = tuple |> Tuple.map (eval state) } }
  | E_Then { a; b } ->
      ignore <| eval state a;
      eval state b
  | E_Stmt { expr } ->
      ignore <| eval state expr;
      { shape = V_Unit }
  | E_Scope { expr } -> eval state expr
  | E_Assign { assignee; value } ->
      let value = eval state value in
      assign state assignee value;
      { shape = V_Unit }
  | E_Apply { f; arg } -> (
      let f = eval state f in
      let arg = eval state arg in
      match f.shape with
      | V_Fn { def; captured } ->
          let arg_bindings = pattern_match arg def.arg in
          let new_state =
            {
              state with
              scope = Scope.with_values ~parent:(Some captured) arg_bindings;
            }
          in
          let result = eval new_state def.body in
          result
      | V_NativeFn f -> f.impl arg
      | _ -> fail "expected fn")
  | E_Ty expr -> { shape = V_Ty (eval_ty state expr) }
  | E_Native { expr } -> (
      match StringMap.find_opt expr state.natives.by_name with
      | Some value -> value
      | None -> fail "no native %S" expr)
  | E_Module { def } ->
      let module_scope = Scope.init ~parent:(Some state.scope) in
      let new_state = { state with scope = module_scope } in
      ignore @@ eval new_state def;
      let fields =
        module_scope.locals.by_symbol |> SymbolMap.to_list
        |> List.map (fun ((symbol : symbol), value) ->
               (Some symbol.name, value))
      in
      { shape = V_Tuple { tuple = fields |> Tuple.of_list } }
  | E_Field { obj; field } -> (
      let obj = eval state obj in
      match obj.shape with
      | V_Tuple { tuple } -> Tuple.get_named field tuple
      | _ -> fail "%a doesnt have fields" Value.print obj)
  | E_UseDotStar { used; bindings } -> (
      let used = eval state used in
      match used.shape with
      | V_Tuple { tuple } ->
          bindings
          |> List.iter (fun (binding : binding) ->
                 let value = tuple |> Tuple.get_named binding.name.name in
                 state.scope |> Scope.add_local binding.name value);
          { shape = V_Unit }
      | _ -> fail "can't use .* %a" Value.print used)

and eval_ty : state -> Expr.ty -> ty =
 fun state expr ->
  match expr.shape with
  | TE_Unit -> Ty.inferred T_Unit
  | TE_Fn { arg; result } ->
      let arg = eval_ty state arg in
      let result = eval_ty state result in
      Ty.inferred (T_Fn { arg; result })
  | TE_Expr expr ->
      let value = eval state expr in
      value |> Value.expect_ty
