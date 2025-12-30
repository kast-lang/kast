open Std
open Kast_util
open Kast_types
open Types
module JsAst = Javascript_ast

type ctx = {
  interpreter : interpreter_state;
  span : span;
  target : value_target;
}

type _ Effect.t += GetCtx : ctx Effect.t

module Impl = struct
  let rec _unused () = ()

  and tuple_field_name : Tuple.member -> string = function
    | Name name -> name
    | Index i -> Int.to_string i

  and fn (def : maybe_compiled_fn) : JsAst.expr =
    let ctx = Effect.perform GetCtx in
    let compiled = def |> Kast_interpreter.await_compiled ~span:ctx.span in
    let { arg; body; evaled_result = _ } =
      compiled |> Option.unwrap_or_else (fun () -> fail "fn not compiled")
    in
    let arg_name = JsAst.gen_name "arg" in
    JsAst.Fn
      {
        args = [ arg_name ];
        body =
          pattern_match arg (place_of (JsAst.Var arg_name))
          @ [ JsAst.Return (transpile_expr body) ];
      }

  and transpile_value : value -> JsAst.expr =
   fun value -> value |> Value.await_inferred |> transpile_value_shape

  and transpile_value_shape : value_shape -> JsAst.expr =
   fun shape ->
    let ctx = Effect.perform GetCtx in
    match shape with
    | V_Unit -> JsAst.Null
    | V_Bool x -> JsAst.Bool x
    | V_Int32 x -> JsAst.Number (Int32.to_float x)
    | V_Int64 x -> JsAst.Number (Int64.to_float x)
    | V_Float64 x -> JsAst.Number x
    | V_Char c -> JsAst.String (String.make 1 c)
    | V_String s -> JsAst.String s
    | V_Ref _ -> failwith __LOC__
    | V_Tuple { tuple; ty = _ } ->
        JsAst.Obj
          (tuple |> Tuple.to_seq
          |> Seq.map (fun (member, (field : value_tuple_field)) ->
              let field_name = tuple_field_name member in
              let value =
                field.place |> Kast_interpreter.read_place ~span:ctx.span
              in
              (field_name, transpile_value value))
          |> List.of_seq)
    | V_Variant _ -> failwith __LOC__
    | V_Ty _ -> JsAst.Null
    | V_Fn { ty = _; fn = { def; _ } } -> fn def
    | V_Generic _ -> failwith __LOC__
    | V_NativeFn _ -> failwith __LOC__
    | V_Ast _ -> failwith __LOC__
    | V_UnwindToken _ -> failwith __LOC__
    | V_Target _ -> failwith __LOC__
    | V_ContextTy _ -> failwith __LOC__
    | V_CompilerScope _ -> failwith __LOC__
    | V_Opaque _ -> failwith __LOC__
    | V_Blocked _ -> failwith __LOC__
    | V_Error -> JsAst.Undefined

  and binding_name : binding -> JsAst.name =
   fun binding ->
    { raw = make_string "%s_%d" binding.name.name binding.id.value }

  and claim : JsAst.expr -> JsAst.expr =
   fun place ->
    JsAst.Call { f = JsAst.Field { obj = place; field = "get" }; args = [] }

  and pattern_match : pattern -> JsAst.expr -> JsAst.stmt list =
   fun pattern place ->
    match pattern.shape with
    | P_Placeholder -> []
    | P_Ref _ -> failwith __LOC__
    | P_Unit -> []
    | P_Binding { bind_mode; binding } -> (
        match bind_mode with
        | Claim ->
            [ JsAst.Let { var = binding_name binding; value = claim place } ]
        | ByRef { mut } -> failwith __LOC__)
    | P_Tuple _ -> failwith __LOC__
    | P_Variant _ -> failwith __LOC__
    | P_Error -> []

  and assign : assignee_expr -> JsAst.expr -> JsAst.stmt list =
   fun assignee place ->
    match assignee.shape with
    | A_Placeholder -> []
    | A_Unit -> []
    | A_Tuple _ -> failwith __LOC__
    | A_Place _ -> failwith __LOC__
    | A_Let pattern -> pattern_match pattern place
    | A_Error -> []

  and stmts_expr : JsAst.stmt list -> JsAst.expr =
   fun stmts ->
    JsAst.Call { f = JsAst.Fn { args = []; body = stmts }; args = [] }

  and place ~get ~set = JsAst.Obj [ ("get", get); ("set", set) ]

  and place_of expr =
    place
      ~get:(JsAst.Fn { args = []; body = [ JsAst.Return expr ] })
      ~set:
        (let var = JsAst.gen_name "value" in
         JsAst.Fn
           {
             args = [ var ];
             body = [ JsAst.Assign { assignee = expr; value = JsAst.Var var } ];
           })

  and scope body = JsAst.Call { f = JsAst.Fn { args = []; body }; args = [] }

  and eval_place : place_expr -> JsAst.expr =
   fun expr ->
    match expr.shape with
    | PE_Binding binding -> place_of (JsAst.Var (binding_name binding))
    | PE_Field _ -> failwith __LOC__
    | PE_Deref _ -> failwith __LOC__
    | PE_Temp expr ->
        let var = JsAst.gen_name "temp" in
        scope
          [
            JsAst.Let { var; value = transpile_expr expr };
            JsAst.Return (place_of (JsAst.Var var));
          ]
    | PE_Error -> failwith __LOC__

  and transpile_expr_as_stmts : expr -> JsAst.stmt list =
   fun expr ->
    let ctx = Effect.perform GetCtx in
    match expr.shape with
    | E_Constant _ -> failwith __LOC__
    | E_Ref _ -> failwith __LOC__
    | E_Claim _ -> failwith __LOC__
    | E_Then _ -> failwith __LOC__
    | E_Stmt _ -> failwith __LOC__
    | E_Scope _ -> failwith __LOC__
    | E_Fn _ -> failwith __LOC__
    | E_Generic _ -> failwith __LOC__
    | E_Tuple _ -> failwith __LOC__
    | E_Variant _ -> failwith __LOC__
    | E_Apply _ -> failwith __LOC__
    | E_InstantiateGeneric _ -> failwith __LOC__
    | E_Assign { assignee; value } ->
        let var = JsAst.gen_name "var" in
        [ JsAst.Let { var; value = eval_place value } ]
        @ assign assignee (JsAst.Var var)
    | E_Ty _ -> failwith __LOC__
    | E_Newtype _ -> failwith __LOC__
    | E_Native _ -> failwith __LOC__
    | E_Module _ -> failwith __LOC__
    | E_UseDotStar { used; bindings } ->
        let used_var = JsAst.gen_name "used" in
        [ JsAst.Let { var = used_var; value = transpile_expr used } ]
        @ (bindings
          |> List.map (fun binding ->
              JsAst.Let
                {
                  var = binding_name binding;
                  value =
                    JsAst.Field
                      { obj = JsAst.Var used_var; field = binding.name.name };
                }))
    | E_If _ -> failwith __LOC__
    | E_And _ -> failwith __LOC__
    | E_Or _ -> failwith __LOC__
    | E_Match _ -> failwith __LOC__
    | E_QuoteAst _ -> failwith __LOC__
    | E_Loop _ -> failwith __LOC__
    | E_Unwindable _ -> failwith __LOC__
    | E_Unwind _ -> failwith __LOC__
    | E_InjectContext _ -> failwith __LOC__
    | E_CurrentContext _ -> failwith __LOC__
    | E_ImplCast _ -> failwith __LOC__
    | E_Cast _ -> failwith __LOC__
    | E_TargetDependent _ -> failwith __LOC__
    | E_Error -> failwith __LOC__

  and transpile_expr : expr -> JsAst.expr =
   fun expr ->
    let ctx = Effect.perform GetCtx in
    match expr.shape with
    | E_Constant value -> transpile_value value
    | E_Ref _ -> failwith __LOC__
    | E_Claim expr -> eval_place expr |> claim
    | E_Then { list } ->
        let body =
          list
          |> List.mapi (fun i e ->
              if i + 1 = List.length list then
                [ JsAst.Return (transpile_expr e) ]
              else transpile_expr_as_stmts e)
          |> List.flatten
        in
        JsAst.Call { f = JsAst.Fn { args = []; body }; args = [] }
    | E_Stmt { expr } -> transpile_expr expr
    | E_Scope { expr } ->
        (* TODO should I actually create js scope? *)
        transpile_expr expr
    | E_Fn { ty = _; def } -> fn def
    | E_Generic _ -> failwith __LOC__
    | E_Tuple _ -> failwith __LOC__
    | E_Variant _ -> failwith __LOC__
    | E_Apply { f; arg } ->
        JsAst.Call { f = transpile_expr f; args = [ transpile_expr arg ] }
    | E_InstantiateGeneric _ -> failwith __LOC__
    | E_Assign _ -> transpile_expr_as_stmts expr |> stmts_expr
    | E_Ty _ -> failwith __LOC__
    | E_Newtype _ -> failwith __LOC__
    | E_Native { id = _; expr } -> JsAst.Raw expr
    | E_Module _ -> failwith __LOC__
    | E_UseDotStar _ -> failwith __LOC__
    | E_If _ -> failwith __LOC__
    | E_And _ -> failwith __LOC__
    | E_Or _ -> failwith __LOC__
    | E_Match _ -> failwith __LOC__
    | E_QuoteAst _ -> failwith __LOC__
    | E_Loop _ -> failwith __LOC__
    | E_Unwindable _ -> failwith __LOC__
    | E_Unwind _ -> failwith __LOC__
    | E_InjectContext _ -> failwith __LOC__
    | E_CurrentContext _ -> failwith __LOC__
    | E_ImplCast _ -> failwith __LOC__
    | E_Cast _ -> failwith __LOC__
    | E_TargetDependent { branches; _ } ->
        let branch =
          Kast_interpreter.find_target_dependent_branch ctx.interpreter branches
            ctx.target
        in
        let branch =
          branch |> Option.unwrap_or_else (fun () -> fail "no js cfg branch")
        in
        transpile_expr branch.body
    | E_Error -> failwith __LOC__
end

type result = { print : formatter -> unit }

let with_ctx ~state ~span f =
  let ctx : ctx =
    { interpreter = state; span; target = { name = "javascript" } }
  in
  try
    let ast = f () in
    { print = (fun fmt -> JsAst.print_expr ~precedence:None fmt ast) }
  with effect GetCtx, k -> Effect.continue k ctx

let transpile_value : state:interpreter_state -> span:span -> value -> result =
 fun ~state ~span value ->
  with_ctx ~state ~span (fun () -> Impl.transpile_value value)

let transpile_expr : state:interpreter_state -> span:span -> expr -> result =
 fun ~state ~span expr ->
  with_ctx ~state ~span (fun () -> Impl.transpile_expr expr)
