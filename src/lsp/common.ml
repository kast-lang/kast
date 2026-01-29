open Std
open Kast_util
open Kast_types
module Lsp = Linol_lsp
module Compiler = Kast_compiler

let span_to_range (span : span) : Lsp.Types.Range.t =
  { start = { line = span.start.line - 1; character = span.start.column - 1 }
  ; end_ = { line = span.finish.line - 1; character = span.finish.column - 1 }
  }
;;

let lsp_to_kast_pos (pos : Lsp.Types.Position.t) : position =
  { (* We dont use index in lsp *)
    index = 0
  ; line = pos.line + 1
  ; column = pos.character + 1
  }
;;

let span_location (span : span) : Lsp.Types.Location.t option =
  let location : Lsp.Types.Location.t =
    { uri = Lsp.Uri0.of_string (Uri.to_string span.uri); range = span_to_range span }
  in
  Some location
;;

let uri_to_lsp (uri : Uri.t) : Lsp.Uri0.t = Lsp.Uri0.of_string (Uri.to_string uri)
let uri_from_lsp (uri : Lsp.Uri0.t) : Uri.t = Uri.of_string (Lsp.Uri0.to_string uri)

type inner_compiled_handler =
  { handle : 'a. 'a compiled_kind -> ?evaled:value -> 'a -> unit }

let inner_tuple_compiled_with_handler =
  fun (type a)
    (kind : a compiled_kind)
    (tuple : a Types.tuple_of)
    (handler : inner_compiled_handler)
    : unit ->
  tuple.parts
  |> List.iter (fun (part : a Types.tuple_part_of) ->
    match part with
    | Field { label = _; label_span = _; field } -> handler.handle kind field
    | Unpack packed -> handler.handle kind packed)
;;

let inner_compiled_with_handler
  : 'a. 'a compiled_kind -> 'a -> inner_compiled_handler -> unit
  =
  fun (type a)
    (kind : a compiled_kind)
    (compiled : a)
    (handler : inner_compiled_handler)
    : unit ->
  (match kind with
   | PlaceExpr ->
     (match compiled.shape with
      | PE_Error -> ()
      | PE_Binding _ -> ()
      | PE_Deref ref -> handler.handle Expr ref
      | PE_Temp expr -> handler.handle Expr expr
      | PE_Field { obj; field; field_span = _ } ->
        handler.handle PlaceExpr obj;
        (match field with
         | Name _ -> ()
         | Index _ -> ()
         | Expr e -> handler.handle Expr e))
   | Expr ->
     (match compiled.shape with
      | E_Ref { mut = _; place } -> handler.handle PlaceExpr place
      | E_Claim place -> handler.handle PlaceExpr place
      | E_Constant _ -> ()
      | E_Then { list } -> list |> List.iter (handler.handle Expr)
      | E_Stmt { expr } -> handler.handle Expr expr
      | E_Scope { expr } -> handler.handle Expr expr
      | E_Fn { def; ty = _ } | E_Generic { def; ty = _ } ->
        (match def.compiled with
         | None -> ()
         | Some { args = { pattern = args }; body } ->
           handler.handle Pattern args;
           handler.handle Expr body)
      | E_Tuple tuple -> inner_tuple_compiled_with_handler kind tuple handler
      | E_Variant { label = _; label_span = _; value } ->
        value |> Option.iter (handler.handle Expr)
      | E_Apply { f; arg } ->
        handler.handle Expr f;
        handler.handle Expr arg
      | E_InstantiateGeneric { generic; arg } ->
        handler.handle Expr generic;
        handler.handle Expr arg
      | E_Assign { assignee; value } ->
        handler.handle Assignee assignee;
        handler.handle PlaceExpr value
      | E_Ty expr -> handler.handle TyExpr expr
      | E_Newtype expr -> handler.handle TyExpr expr
      | E_Native _ -> ()
      | E_Module { def; bindings = _ } -> handler.handle Expr def
      | E_UseDotStar { used; bindings = _ } -> handler.handle Expr used
      | E_If { cond; then_case; else_case } ->
        handler.handle Expr cond;
        handler.handle Expr then_case;
        handler.handle Expr else_case
      | E_And { lhs; rhs } | E_Or { lhs; rhs } ->
        handler.handle Expr lhs;
        handler.handle Expr rhs
      | E_Match { value; branches } ->
        handler.handle PlaceExpr value;
        branches
        |> List.iter
             (fun ({ pattern = _; body = _ } as branch : Types.expr_match_branch) ->
                handler.handle Pattern branch.pattern;
                handler.handle Expr branch.body)
      | E_Loop { body } -> handler.handle Expr body
      | E_QuoteAst _ ->
        (* TODO *)
        ()
      | E_Unwindable { token; body } ->
        handler.handle Pattern token;
        handler.handle Expr body
      | E_Unwind { token; value } ->
        handler.handle Expr token;
        handler.handle Expr value
      | E_TargetDependent { branches; captured = _; interpreter_branch = _ } ->
        branches
        |> List.iter
             (fun ({ cond; body } : Kast_types.Types.expr_target_dependent_branch) ->
                handler.handle Expr cond;
                handler.handle Expr body)
      | E_InjectContext { context_ty = _; value } -> handler.handle Expr value
      | E_CurrentContext { context_ty = _ } -> ()
      | E_ImplCast { value; target; impl } ->
        handler.handle Expr value;
        let _ : value = target in
        (* handler.handle Expr target; *)
        handler.handle Expr impl
      | E_Cast { value; target } ->
        handler.handle Expr value;
        let _ : value = target in
        ()
      | E_Error -> ())
   | Assignee ->
     (match compiled.shape with
      | A_Placeholder -> ()
      | A_Unit -> ()
      | A_Place place -> handler.handle PlaceExpr place
      | A_Tuple tuple -> inner_tuple_compiled_with_handler kind tuple handler
      | A_Let pattern -> handler.handle Pattern pattern
      | A_Error -> ())
   | Pattern ->
     (match compiled.shape with
      | P_Placeholder -> ()
      | P_Unit -> ()
      | P_Ref inner -> handler.handle Pattern inner
      | P_Binding _ -> ()
      | P_Tuple tuple -> inner_tuple_compiled_with_handler kind tuple handler
      | P_Variant { label = _; label_span = _; value } ->
        value |> Option.iter (handler.handle Pattern)
      | P_Error -> ())
   | TyExpr ->
     (match compiled.compiled_shape with
      | None -> ()
      | Some shape ->
        (match shape with
         | TE_Unit -> ()
         | TE_Ref { mut = _; referenced } -> handler.handle TyExpr referenced
         | TE_Fn { arg; result } ->
           handler.handle TyExpr arg;
           handler.handle TyExpr result
         | TE_Expr expr -> handler.handle Expr expr
         | TE_Tuple tuple -> inner_tuple_compiled_with_handler kind tuple handler
         | TE_Union { elements } -> elements |> List.iter (handler.handle TyExpr)
         | TE_Variant { variants } ->
           variants
           |> List.iter
                (fun
                    ({ label_span = _; label = _; value = variant_data } :
                      Types.ty_expr_variant_variant)
                   -> variant_data |> Option.iter (handler.handle TyExpr))
         | TE_Error -> ())));
  let data = Compiler.get_data kind compiled in
  let { exprs; patterns; ty_exprs; ty_ascribed = _; value = _; binding = _ }
    : Types.ir_evaled
    =
    data.evaled
  in
  exprs |> List.iter (fun (expr, value) -> handler.handle Expr ~evaled:value expr);
  ty_exprs |> List.iter (fun (expr, _ty) -> handler.handle TyExpr expr);
  patterns |> List.iter (handler.handle Pattern)
;;

type _ Effect.t += Yield : (compiled * evaled:value option) -> unit Effect.t

let inner_compiled =
  fun (type a)
    (kind : a compiled_kind)
    (compiled : a)
    : (compiled * evaled:value option) Seq.t ->
  let next : ((compiled * evaled:value option) * (unit, unit) continuation) option ref =
    ref None
  in
  (try
     inner_compiled_with_handler
       kind
       compiled
       { handle =
           (fun (type b)
             (kind : b compiled_kind)
             ?(evaled : value option)
             (compiled : b) ->
             Effect.perform (Yield (Compiled (kind, compiled), ~evaled)))
       }
   with
   | effect Yield compiled, k -> next := Some (compiled, k));
  Seq.of_dispenser (fun () ->
    match !next with
    | Some (compiled, k) ->
      next := None;
      Effect.continue k ();
      Some compiled
    | None -> None)
;;
