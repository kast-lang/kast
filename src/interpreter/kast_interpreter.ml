open Std
open Kast_util
open Kast_types

type bindings = value StringMap.t
type scope = { bindings : bindings }

type state = {
  natives : Natives.t;
  mutable scope : scope;
}

let add_bindings =
  StringMap.union (fun _name _old_value new_value -> Some new_value)

let init : value StringMap.t -> state =
 fun bindings -> { scope = { bindings }; natives = Natives.natives }

let default () = init StringMap.empty

let pattern_match : value -> pattern -> bindings =
 fun value pattern ->
  match pattern.shape with
  | P_Placeholder -> StringMap.empty
  | P_Unit ->
      (* TODO assert that value is unit *)
      StringMap.empty
  | P_Binding binding -> StringMap.singleton binding.name value

let assign : state -> Expr.assignee -> value -> unit =
 fun state assignee value ->
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit ->
      (* TODO assert that value is unit *)
      ()
  | A_Binding { name; ty = _; span = _; references = _ } ->
      if StringMap.find_opt name state.scope.bindings |> Option.is_none then
        fail "trying to assign to undefined variable %S" name;
      state.scope <-
        {
          bindings =
            add_bindings state.scope.bindings (StringMap.singleton name value);
        }
  | A_Let pattern ->
      let new_bindings = pattern_match value pattern in
      state.scope <-
        { bindings = add_bindings state.scope.bindings new_bindings }

let rec eval : state -> expr -> value =
 fun state expr ->
  match expr.shape with
  | E_Constant value -> value
  | E_Binding binding ->
      StringMap.find_opt binding.name state.scope.bindings
      |> Option.unwrap_or_else (fun () -> fail "%S not found" binding.name)
  | E_Fn fn -> { shape = V_Fn fn }
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
      | V_Fn f ->
          let new_bindings = pattern_match arg f.arg in
          let new_state =
            {
              state with
              scope =
                { bindings = add_bindings state.scope.bindings new_bindings };
            }
          in
          let result = eval new_state f.body in
          result
      | V_NativeFn f -> f.impl arg
      | _ -> fail "expected fn")
  | E_Ty expr -> { shape = V_Ty (eval_ty state expr) }
  | E_Native { expr } -> (
      match StringMap.find_opt expr state.natives.by_name with
      | Some value -> value
      | None -> fail "no native %S" expr)
  | E_Module { def } ->
      let new_state = { state with scope = { bindings = StringMap.empty } } in
      ignore @@ eval new_state def;
      let fields =
        new_state.scope.bindings |> StringMap.to_list
        |> List.map (fun (name, value) -> (Some name, value))
      in
      { shape = V_Tuple { tuple = fields |> Tuple.of_list } }

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
