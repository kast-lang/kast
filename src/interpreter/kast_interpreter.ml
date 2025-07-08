open Std
open Kast_util
open Kast_types
module Ast = Kast_ast
module Error = Error

type state = {
  natives : Natives.t;
  scope : Scope.t;
}

let init : Scope.locals -> state =
 fun values ->
  { scope = Scope.with_values ~parent:None values; natives = Natives.natives }

let default () = init Scope.Locals.empty

let rec pattern_match : value -> pattern -> Scope.locals =
 fun value pattern ->
  match pattern.shape with
  | P_Placeholder -> Scope.Locals.empty
  | P_Unit ->
      (* TODO assert that value is unit *)
      Scope.Locals.empty
  | P_Binding binding -> { by_symbol = SymbolMap.singleton binding.name value }
  | P_Tuple { tuple = tuple_pattern } -> (
      match value.shape with
      | V_Tuple { tuple } ->
          {
            by_symbol =
              Tuple.zip_order_a tuple_pattern tuple
              |> Tuple.to_seq
              |> Seq.fold_left
                   (fun acc (_member, (field_pattern, field_value)) ->
                     let field_matches =
                       pattern_match field_value field_pattern
                     in
                     SymbolMap.union
                       (fun symbol _a b ->
                         Error.error pattern.data.span
                           "multiple bindings of same symbol %a" Symbol.print
                           symbol;
                         Some b)
                       acc field_matches.by_symbol)
                   SymbolMap.empty;
          }
      | _ ->
          Error.error pattern.data.span "Expected tuple, got %a" Value.print
            value;
          Scope.Locals.empty)
  | P_Error -> Scope.Locals.empty

let assign : state -> Expr.assignee -> value -> unit =
 fun state assignee value ->
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit ->
      (* TODO assert that value is unit *)
      ()
  | A_Binding { name; ty = _; span = _; references = _ } ->
      state.scope
      |> Scope.assign_to_existing ~span:assignee.data.span name value
  | A_Let pattern ->
      let new_bindings = pattern_match value pattern in
      state.scope |> Scope.add_locals new_bindings
  | A_Error -> ()

let rec eval : state -> expr -> value =
 fun state expr ->
  match expr.shape with
  | E_Constant value -> value
  | E_Binding binding ->
      Scope.find_opt binding.name state.scope
      |> Option.unwrap_or_else (fun () : value ->
             Log.trace "all in scope: %a" Scope.print_all state.scope;
             Error.error expr.data.span "%a not found" Symbol.print binding.name;
             { shape = V_Error })
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
      | V_NativeFn f -> f.impl ~caller:expr.data.span arg
      | _ ->
          Error.error expr.data.span "expected fn";
          { shape = V_Error })
  | E_Ty expr -> { shape = V_Ty (eval_ty state expr) }
  | E_Native { expr = native_expr } -> (
      match StringMap.find_opt native_expr state.natives.by_name with
      | Some value -> value
      | None ->
          Error.error expr.data.span "no native %S" native_expr;
          { shape = V_Error })
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
      | V_Tuple { tuple } -> (
          match Tuple.get_named_opt field tuple with
          | Some field -> field
          | None ->
              Error.error expr.data.span "field %S doesnt exist" field;
              { shape = V_Error })
      | _ ->
          Error.error expr.data.span "%a doesnt have fields" Value.print obj;
          { shape = V_Error })
  | E_UseDotStar { used; bindings } -> (
      let used = eval state used in
      match used.shape with
      | V_Tuple { tuple } ->
          bindings
          |> List.iter (fun (binding : binding) ->
                 let value = tuple |> Tuple.get_named binding.name.name in
                 state.scope |> Scope.add_local binding.name value);
          { shape = V_Unit }
      | _ ->
          Error.error expr.data.span "can't use .* %a" Value.print used;
          { shape = V_Error })
  | E_If { cond = cond_expr; then_case; else_case } -> (
      let cond = eval state cond_expr in
      match cond.shape with
      | V_Bool true -> eval state then_case
      | V_Bool false -> eval state else_case
      | _ ->
          Error.error cond_expr.data.span "if cond must be bool, got %a"
            Value.print cond;
          { shape = V_Error })
  | E_QuoteAst expr -> { shape = V_Ast (quote_ast state expr) }
  | E_Error -> { shape = V_Error }

and quote_ast : state -> Expr.Shape.quote_ast -> Ast.t =
 fun state expr ->
  let rec quote_group (group : Expr.Shape.quote_ast_group) : Ast.group =
    {
      rule = group.rule;
      parts = [];
      children =
        group.children
        |> Tuple.map (fun (child : Expr.Shape.quote_ast_child) : Ast.child ->
               match child with
               | Group child_group -> Group (quote_group child_group)
               | Ast child ->
                   Ast
                     (let child = eval state child in
                      match child.shape with
                      | V_Ast ast -> ast
                      | _ -> fail "child must be ast"));
    }
  in
  {
    shape = Complex { rule = expr.rule; root = quote_group expr.root };
    span = Span.fake "todo";
  }

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
  | TE_Error -> Ty.inferred T_Error
