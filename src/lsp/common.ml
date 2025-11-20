open Std
open Kast_util
open Kast_types
module Lsp = Linol_lsp
module Compiler = Kast_compiler

type 'a compiled_kind = 'a Compiler.compiled_kind

let span_to_range (span : span) : Lsp.Types.Range.t =
  {
    start = { line = span.start.line - 1; character = span.start.column - 1 };
    end_ = { line = span.finish.line - 1; character = span.finish.column - 1 };
  }

let lsp_to_kast_pos (pos : Lsp.Types.Position.t) : position =
  {
    (* We dont use index in lsp *)
    index = 0;
    line = pos.line + 1;
    column = pos.character + 1;
  }

let span_location (span : span) : Lsp.Types.Location.t option =
  let location : Lsp.Types.Location.t =
    {
      uri = Lsp.Uri0.of_string (Uri.to_string span.uri);
      range = span_to_range span;
    }
  in
  Some location

let uri_to_lsp (uri : Uri.t) : Lsp.Uri0.t =
  Lsp.Uri0.of_string (Uri.to_string uri)

let uri_from_lsp (uri : Lsp.Uri0.t) : Uri.t =
  Uri.of_string (Lsp.Uri0.to_string uri)

type inner_compiled_handler = { handle : 'a. 'a compiled_kind -> 'a -> unit }

let inner_compiled_with_handler =
 fun (type a) (kind : a compiled_kind) (compiled : a)
     (handler : inner_compiled_handler) : unit ->
  (match kind with
  | Expr -> (
      match compiled.shape with
      | E_Constant _ -> ()
      | E_Binding _ -> ()
      | E_Then { a; b } ->
          handler.handle Expr a;
          handler.handle Expr b
      | E_Stmt { expr } -> handler.handle Expr expr
      | E_Scope { expr } -> handler.handle Expr expr
      | E_Fn { def; ty = _ } | E_Generic { def } -> (
          match def.compiled with
          | None -> ()
          | Some { arg; body; evaled_result } -> (
              handler.handle Pattern arg;
              handler.handle Expr body;
              match evaled_result with
              | Some expr -> handler.handle TyExpr expr
              | None -> ()))
      | E_Tuple { tuple } ->
          tuple |> Tuple.to_seq
          |> Seq.iter
               (fun (_member, (~field_span:_, ~field_label:_, field_expr)) ->
                 handler.handle Expr field_expr)
      | E_Apply { f; arg } ->
          handler.handle Expr f;
          handler.handle Expr arg
      | E_InstantiateGeneric { generic; arg } ->
          handler.handle Expr generic;
          handler.handle Expr arg
      | E_Assign { assignee; value } ->
          handler.handle Assignee assignee;
          handler.handle Expr value
      | E_Ty expr -> handler.handle TyExpr expr
      | E_Native _ -> ()
      | E_Module { def } -> handler.handle Expr def
      | E_Field { obj; field = _; field_span = _; label = _ } ->
          handler.handle Expr obj
      | E_UseDotStar { used; bindings = _ } -> handler.handle Expr used
      | E_If { cond; then_case; else_case } ->
          handler.handle Expr cond;
          handler.handle Expr then_case;
          handler.handle Expr else_case
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
      | E_TargetDependent { branches } ->
          branches
          |> List.iter
               (fun
                 ({ cond; body } :
                   Kast_types.Types.expr_target_dependent_branch)
               ->
                 handler.handle Expr cond;
                 handler.handle Expr body)
      | E_InjectContext { context_ty = _; value } -> handler.handle Expr value
      | E_CurrentContext { context_ty = _ } -> ()
      | E_Error -> ())
  | Assignee -> (
      match compiled.shape with
      | A_Placeholder -> ()
      | A_Unit -> ()
      | A_Binding _ -> ()
      | A_Let pattern -> handler.handle Pattern pattern
      | A_Error -> ())
  | Pattern -> (
      match compiled.shape with
      | P_Placeholder -> ()
      | P_Unit -> ()
      | P_Binding _ -> ()
      | P_Tuple { tuple } ->
          tuple |> Tuple.to_seq
          |> Seq.iter
               (fun (_member, (~field_span:_, ~field_label:_, field_pattern)) ->
                 handler.handle Pattern field_pattern)
      | P_Error -> ())
  | TyExpr -> (
      match compiled.shape with
      | TE_Unit -> ()
      | TE_Fn { arg; result } ->
          handler.handle TyExpr arg;
          handler.handle TyExpr result
      | TE_Expr expr -> handler.handle Expr expr
      | TE_Tuple { tuple } ->
          tuple |> Tuple.to_seq
          |> Seq.iter
               (fun (_member, (~field_span:_, ~field_label:_, field_expr)) ->
                 handler.handle TyExpr field_expr)
      | TE_Error -> ()));
  let data = Compiler.get_data kind compiled in
  data.evaled_exprs |> List.iter (fun expr -> handler.handle Expr expr);
  match data.ty_ascription with
  | Some ty_expr -> handler.handle TyExpr ty_expr
  | None -> ()

type compiled_thing =
  | CompiledThing : 'a. 'a compiled_kind * 'a -> compiled_thing

type _ Effect.t += Yield : compiled_thing -> unit Effect.t

let inner_compiled =
 fun (type a) (kind : a compiled_kind) (compiled : a) : compiled_thing Seq.t ->
  let next : (compiled_thing * (unit, unit) continuation) option ref =
    ref None
  in
  (try
     inner_compiled_with_handler kind compiled
       {
         handle =
           (fun (type b) (kind : b compiled_kind) (compiled : b) ->
             Effect.perform (Yield (CompiledThing (kind, compiled))));
       }
   with effect Yield compiled_thing, k -> next := Some (compiled_thing, k));
  Seq.of_dispenser (fun () ->
      match !next with
      | Some (compiled_thing, k) ->
          next := None;
          Effect.continue k ();
          Some compiled_thing
      | None -> None)
