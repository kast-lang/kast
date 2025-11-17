open Std
open Kast_util
open Kast_types
module Inference = Kast_inference_base
module OcamlAst = Ocaml_ast

module State = struct
  type t = { mutable types : OcamlAst.t StringMap.t }

  let init () : t = { types = StringMap.empty }
end

type state = State.t

let binding_name (binding : binding) : string = binding.name.name
let field_name (field : string) : string = field
let interpreter_cached = ref None

let interpreter () =
  match !interpreter_cached with
  | Some x -> x
  | None ->
      let x = (Kast_compiler.default ()).interpreter in
      interpreter_cached := Some x;
      x

let rec _unused = ()
and transpile_module : Value.t -> OcamlAst.t = fun _ -> failwith __LOC__

and transpile_tuple :
    'a. ('a -> state -> OcamlAst.t) -> 'a tuple -> state -> OcamlAst.t =
 fun transpile_a tuple state ->
  OcamlAst.Tuple
    (let { unnamed; named; named_order_rev = _ } : 'a tuple = tuple in
     let unnamed =
       unnamed |> Array.to_list |> List.map (fun a -> state |> transpile_a a)
     in
     let named =
       named |> StringMap.to_list |> List.map snd
       |> List.map (fun a -> state |> transpile_a a)
     in
     unnamed @ named)

and transpile_value : Value.t -> state -> OcamlAst.t =
 fun value state ->
  match value.shape with
  | Types.V_Unit -> OcamlAst.unit_value
  | Types.V_Bool value -> OcamlAst.Bool value
  | Types.V_Int32 value -> OcamlAst.Int32 value
  | Types.V_String value -> OcamlAst.String value
  | Types.V_Tuple tuple ->
      state
      |> transpile_tuple
           (fun (field : Types.value_tuple_field) ->
             transpile_value field.value)
           tuple.tuple
  | Types.V_Ty _ -> OcamlAst.unit_value
  | Types.V_Fn { def; captured = _ } ->
      OcamlAst.Fun
        {
          args = [ state |> transpile_pattern def.arg ];
          body = state |> transpile_expr def.body;
        }
  | Types.V_NativeFn { name; _ } ->
      fail "Tried to transpile interpreter native fn %S" name
  | Types.V_Ast _ -> failwith __LOC__
  | Types.V_UnwindToken _ -> failwith __LOC__
  | Types.V_Target _ -> fail "Tried to transpile target value"
  | Types.V_Error -> fail "Tried to transpile error node"

and transpile_pattern : Pattern.t -> state -> OcamlAst.t =
 fun pattern state ->
  match pattern.shape with
  | Types.P_Placeholder -> OcamlAst.Placeholder
  | Types.P_Unit -> OcamlAst.unit_value
  | Types.P_Binding binding -> OcamlAst.Var (binding_name binding)
  | Types.P_Tuple tuple ->
      state
      |> transpile_tuple
           (fun (~field_span:_, ~field_label:_, field_pattern) ->
             transpile_pattern field_pattern)
           tuple.tuple
  | Types.P_Error -> fail "Tried to transpile error node"

and transpile_expr : Expr.t -> state -> OcamlAst.t =
 fun expr state ->
  match expr.shape with
  | Types.E_Constant value -> state |> transpile_value value
  | Types.E_Binding binding -> OcamlAst.Var (binding_name binding)
  | Types.E_Then { a; b } ->
      let a = state |> transpile_expr a in
      let b = state |> transpile_expr b in
      OcamlAst.merge_let_then a b
  | Types.E_Stmt { expr } ->
      with_return (fun { return } ->
          let expr = state |> transpile_expr expr in
          (match expr with
          | OcamlAst.LetThen parts -> (
              match parts |> List.last with
              | Let _ -> return expr
              | _ -> ())
          | _ -> ());
          OcamlAst.single_let { pattern = OcamlAst.Placeholder; value = expr })
  | Types.E_Scope { expr } -> OcamlAst.Scope (state |> transpile_expr expr)
  | Types.E_Fn def ->
      OcamlAst.Fun
        {
          args = [ state |> transpile_pattern def.arg ];
          body = state |> transpile_expr def.body;
        }
  | Types.E_Tuple tuple ->
      state
      |> transpile_tuple
           (fun (~field_span:_, ~field_label:_, field_expr) ->
             transpile_expr field_expr)
           tuple.tuple
  | Types.E_Apply { f; arg } ->
      OcamlAst.Call
        { f = state |> transpile_expr f; arg = state |> transpile_expr arg }
  | Types.E_Assign { assignee; value } ->
      let value_ident = "_value" in
      let rec perform_assign (assignee : Expr.assignee) (value : OcamlAst.t) :
          OcamlAst.t =
        match assignee.shape with
        | Types.A_Placeholder | Types.A_Unit -> OcamlAst.unit_value
        | Types.A_Binding _ -> fail "todo support mutation"
        | Types.A_Let pattern ->
            OcamlAst.single_let
              { pattern = state |> transpile_pattern pattern; value }
        | Types.A_Error -> fail "Tried to transpile error node"
      in
      OcamlAst.merge_let_then
        (OcamlAst.single_let
           {
             pattern = OcamlAst.Var value_ident;
             value = state |> transpile_expr value;
           })
        (perform_assign assignee (OcamlAst.Var value_ident))
  | Types.E_Ty _ -> fail "Tried to transpile type expr"
  | Types.E_Native { expr } ->
      OcamlAst.RawCode
        (expr |> String.to_seq
        |> Seq.filter (fun c -> c <> '{' && c <> '}')
        |> String.of_seq)
  | Types.E_Module { def } ->
      OcamlAst.Scope
        (let def = state |> transpile_expr def in
         let module_ty = expr.data.ty.var |> Inference.Var.await_inferred in
         let module_result =
           match module_ty with
           | Types.T_Tuple { tuple } ->
               assert (tuple.unnamed |> Array.length = 0);
               OcamlAst.Tuple
                 (tuple.named |> StringMap.to_list
                 |> List.map (fun (name, _ty) -> OcamlAst.Var name))
           | _ -> fail "module ty is not tuple???"
         in
         OcamlAst.merge_let_then def module_result)
  | Types.E_Field { obj; field; field_span = _; label = _ } -> (
      let obj_ty = obj.data.ty.var |> Inference.Var.await_inferred in
      match obj_ty with
      | Types.T_Tuple { tuple = _ } ->
          OcamlAst.Field
            { obj = state |> transpile_expr obj; field = field_name field }
      | _ -> fail "trying to get field of not a tuple: %a" Ty.Shape.print obj_ty
      )
  | Types.E_UseDotStar { used; bindings } ->
      OcamlAst.LetThen
        [
          OcamlAst.Let
            {
              recursive = false;
              bindings =
                [
                  {
                    pattern = OcamlAst.Var "_used";
                    value = state |> transpile_expr used;
                  };
                ];
            };
          OcamlAst.Let
            {
              recursive = false;
              bindings =
                bindings
                |> List.map (fun binding : OcamlAst.let_binding ->
                    let name = binding_name binding in
                    {
                      pattern = OcamlAst.Var name;
                      value =
                        OcamlAst.Field
                          { obj = OcamlAst.Var "_used"; field = name };
                    });
            };
        ]
  | Types.E_If { cond; then_case; else_case } ->
      OcamlAst.If
        {
          cond = state |> transpile_expr cond;
          then_case = state |> transpile_expr then_case;
          else_case = state |> transpile_expr else_case;
        }
  | Types.E_QuoteAst _ -> fail "Tried to compile quote ast"
  | Types.E_Loop _ -> failwith __LOC__
  | Types.E_Unwindable _ -> failwith __LOC__
  | Types.E_Unwind _ -> failwith __LOC__
  | Types.E_TargetDependent { branches } ->
      let branch =
        Kast_interpreter.find_target_dependent_branch (interpreter ()) branches
          { name = "ocaml" }
        |> Option.unwrap_or_else (fun () ->
            fail "no branch for ocaml %a" Span.print expr.data.span)
      in
      state |> transpile_expr branch.body
  | Types.E_Error -> fail "Tried to transpile error node"

module Full = struct
  let transpile_expr expr : OcamlAst.t =
    let state = State.init () in
    let expr = transpile_expr expr state in
    (* TODO types *)
    expr
end
