open Std
open Kast_util
open Kast_types
module Inference = Kast_inference_base
module OcamlAst = Ocaml_ast

let binding_name (binding : binding) : string = binding.name.name

let get_tuple_field ~(index : int) ~(length : int) (expr : OcamlAst.t) :
    OcamlAst.t =
  OcamlAst.Match
    {
      expr;
      branches =
        [
          {
            pattern =
              OcamlAst.Tuple
                (List.init length (fun i ->
                     if i = index then OcamlAst.Var "_field"
                     else OcamlAst.Placeholder));
            body = OcamlAst.Var "_field";
          };
        ];
    }

let rec _unused = ()
and transpile_module : Value.t -> OcamlAst.t = fun _ -> failwith __LOC__

and transpile_tuple : 'a. ('a -> OcamlAst.t) -> 'a tuple -> OcamlAst.t =
 fun transpile_a tuple ->
  OcamlAst.Tuple
    (let { unnamed; named; named_order_rev = _ } : 'a tuple = tuple in
     let unnamed = unnamed |> Array.to_list |> List.map transpile_a in
     let named =
       named |> StringMap.to_list |> List.map snd |> List.map transpile_a
     in
     unnamed @ named)

and transpile_value : Value.t -> OcamlAst.t =
 fun value ->
  match value.shape with
  | Types.V_Unit -> OcamlAst.unit_value
  | Types.V_Bool value -> OcamlAst.Bool value
  | Types.V_Int32 value -> OcamlAst.Int32 value
  | Types.V_String value -> OcamlAst.String value
  | Types.V_Tuple tuple -> transpile_tuple transpile_value tuple.tuple
  | Types.V_Ty _ -> fail "Tried to transpile type"
  | Types.V_Fn { def; captured = _ } ->
      OcamlAst.Fun
        { args = [ transpile_pattern def.arg ]; body = transpile_expr def.body }
  | Types.V_NativeFn _ -> fail "Tried to transpile interpreter native fn"
  | Types.V_Ast _ -> failwith __LOC__
  | Types.V_UnwindToken _ -> failwith __LOC__
  | Types.V_Error -> fail "Tried to transpile error node"

and transpile_pattern : Pattern.t -> OcamlAst.t =
 fun pattern ->
  match pattern.shape with
  | Types.P_Placeholder -> OcamlAst.Placeholder
  | Types.P_Unit -> OcamlAst.unit_value
  | Types.P_Binding binding -> OcamlAst.Var (binding_name binding)
  | Types.P_Tuple tuple -> transpile_tuple transpile_pattern tuple.tuple
  | Types.P_Error -> fail "Tried to transpile error node"

and transpile_expr : Expr.t -> OcamlAst.t =
 fun expr ->
  match expr.shape with
  | Types.E_Constant value -> transpile_value value
  | Types.E_Binding binding -> OcamlAst.Var (binding_name binding)
  | Types.E_Then { a; b } ->
      let a = transpile_expr a in
      let b = transpile_expr b in
      OcamlAst.merge_let_then a b
  | Types.E_Stmt { expr } ->
      OcamlAst.Call { f = OcamlAst.Var "ignore"; arg = transpile_expr expr }
  | Types.E_Scope { expr } -> OcamlAst.Scope (transpile_expr expr)
  | Types.E_Fn def ->
      OcamlAst.Fun
        { args = [ transpile_pattern def.arg ]; body = transpile_expr def.body }
  | Types.E_Tuple tuple -> transpile_tuple transpile_expr tuple.tuple
  | Types.E_Apply { f; arg } ->
      OcamlAst.Call { f = transpile_expr f; arg = transpile_expr arg }
  | Types.E_Assign { assignee; value } ->
      let value_ident = "_value" in
      let rec perform_assign (assignee : Expr.assignee) (value : OcamlAst.t) :
          OcamlAst.t =
        match assignee.shape with
        | Types.A_Placeholder | Types.A_Unit -> OcamlAst.unit_value
        | Types.A_Binding _ -> fail "todo support mutation"
        | Types.A_Let pattern ->
            OcamlAst.single_let { pattern = transpile_pattern pattern; value }
        | Types.A_Error -> fail "Tried to transpile error node"
      in
      OcamlAst.merge_let_then
        (OcamlAst.single_let
           { pattern = OcamlAst.Var value_ident; value = transpile_expr value })
        (perform_assign assignee (OcamlAst.Var value_ident))
  | Types.E_Ty _ -> fail "Tried to transpile type expr"
  | Types.E_Native _ -> failwith __LOC__
  | Types.E_Module { def } ->
      OcamlAst.Scope
        (let def = transpile_expr def in
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
  | Types.E_Field { obj; field } -> (
      let obj_ty = obj.data.ty.var |> Inference.Var.await_inferred in
      match obj_ty with
      | Types.T_Tuple { tuple } ->
          let named_index =
            tuple.named |> StringMap.to_seq
            |> Seq.find_index (fun (key, _) -> key = field)
          in
          let named_index =
            named_index
            |> Option.unwrap_or_else (fun _ ->
                   fail "No field %s in %a" field Ty.Shape.print obj_ty)
          in
          let field_index = Array.length tuple.unnamed + named_index in
          get_tuple_field ~index:field_index
            ~length:(Array.length tuple.unnamed + StringMap.cardinal tuple.named)
            (transpile_expr obj)
      | _ -> fail "trying to get field of not a tuple: %a" Ty.Shape.print obj_ty
      )
  | Types.E_UseDotStar _ -> OcamlAst.unit_value
  | Types.E_If { cond; then_case; else_case } ->
      OcamlAst.If
        {
          cond = transpile_expr cond;
          then_case = transpile_expr then_case;
          else_case = transpile_expr else_case;
        }
  | Types.E_QuoteAst _ -> fail "Tried to compile quote ast"
  | Types.E_Loop _ -> failwith __LOC__
  | Types.E_Unwindable _ -> failwith __LOC__
  | Types.E_Unwind _ -> failwith __LOC__
  | Types.E_Error -> fail "Tried to transpile error node"
