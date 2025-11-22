open Std
open Kast_util
open Types

module Impl = struct
  let rec _unused = ()

  (* VALUE *)
  and print_value_shape :
      cache:RecurseCache.t -> formatter -> value_shape -> unit =
   fun ~cache fmt -> function
    | V_Unit -> fprintf fmt "()"
    | V_Bool value -> fprintf fmt "%B" value
    | V_Ty ty -> print_ty ~cache fmt ty
    | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
    | V_Char value -> fprintf fmt "@{<green>%C@}" value
    | V_String value -> fprintf fmt "@{<green>%S@}" value
    | V_Tuple { tuple } ->
        fprintf fmt "%a"
          (Tuple.print (fun fmt (field : value_tuple_field) ->
               print_value ~cache fmt field.value))
          tuple
    | V_Fn _ -> fprintf fmt "@{<italic><fn>@}"
    | V_Variant { label; data; ty = _ } -> (
        fprintf fmt ":%a" Label.print label;
        match data with
        | Some data -> fprintf fmt " %a" (print_value ~cache) data
        | None -> ())
    | V_Generic _ -> fprintf fmt "@{<italic><generic>@}"
    | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name
    | V_Ast ast -> fprintf fmt "%a" Ast.print ast
    | V_UnwindToken { id; result_ty = _ } ->
        fprintf fmt "<unwind %a>" Id.print id
    | V_ContextTy ty -> print_context_type ~cache fmt ty
    | V_Error -> fprintf fmt "@{<red><error>@}"
    | V_Binding binding ->
        fprintf fmt "<binding %a>" (print_binding ~cache) binding
    | V_CompilerScope _ -> fprintf fmt "@{<italic><compiler scope>@}"
    | V_Target target -> print_target ~cache fmt target

  and print_value : cache:RecurseCache.t -> formatter -> value -> unit =
   fun ~cache fmt { shape } -> print_value_shape ~cache fmt shape

  (* TY *)
  and print_ty_shape : cache:RecurseCache.t -> formatter -> ty_shape -> unit =
   fun ~cache fmt -> function
    | T_Unit -> fprintf fmt "()"
    | T_Bool -> fprintf fmt "bool"
    | T_Int32 -> fprintf fmt "int32"
    | T_Char -> fprintf fmt "char"
    | T_String -> fprintf fmt "string"
    | T_Variant { variants } ->
        fprintf fmt "%a"
          (Row.print
             ~options:
               {
                 open_ = "";
                 before_label = ":";
                 between = "";
                 rest = "...";
                 sep = " | ";
                 close = "";
               } (fun fmt ({ data } : ty_variant_data) ->
               match data with
               | Some data -> fprintf fmt " %a" (print_ty ~cache) data
               | None -> ()))
          variants
    | T_Tuple { tuple } ->
        fprintf fmt "%a"
          (Tuple.print
             ~options:
               { Tuple.default_print_options with named_field_middle = " :: " }
             (fun fmt (field : ty_tuple_field) -> print_ty ~cache fmt field.ty))
          tuple
    | T_Ty -> fprintf fmt "type"
    | T_Fn { arg; result } ->
        fprintf fmt "@[<hv>%a@] -> @[<hv>%a@]" (print_ty ~cache) arg
          (print_ty ~cache) result
    | T_Generic { def = { compiled; on_compiled = _ } } -> (
        match compiled with
        | None -> fprintf fmt "@{<italic><generic>@}"
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt "[%a] %a" (print_pattern ~cache) arg (print_ty ~cache)
              body.data.ty)
    | T_Ast -> fprintf fmt "ast"
    | T_UnwindToken { result } ->
        fprintf fmt "<unwind %a>" (print_ty ~cache) result
    | T_Target -> fprintf fmt "target"
    | T_ContextTy -> fprintf fmt "context_type"
    | T_CompilerScope -> fprintf fmt "<compiler scope>"
    | T_Binding binding -> fprintf fmt "%a" (print_binding ~cache) binding
    | T_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty : cache:RecurseCache.t -> formatter -> ty -> unit =
   fun ~cache fmt { var } ->
    if cache |> RecurseCache.visit (Inference.Var.recurse_id var) then (
      fprintf fmt "<id=%a>" Id.print (Inference.Var.recurse_id var);
      Inference.Var.print (print_ty_shape ~cache) fmt var)
    else fprintf fmt "<recursive %a>" Id.print (Inference.Var.recurse_id var)

  (* EXPR *)
  and print_expr_shape :
      cache:RecurseCache.t ->
      (formatter -> expr -> unit) ->
      formatter ->
      expr_shape ->
      unit =
   fun ~cache print_expr fmt -> function
    | E_Constant value ->
        fprintf fmt "@{<magenta>const@} %a" (print_value ~cache) value
    | E_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" (print_binding ~cache) binding
    | E_Then { a; b } ->
        fprintf fmt "@{<magenta>then@} %a" (Tuple.print print_expr)
          (Tuple.make [ a; b ] [])
    | E_Stmt { expr } ->
        fprintf fmt "@{<magenta>stmt@} %a" (Tuple.print print_expr)
          (Tuple.make [ expr ] [])
    | E_Scope { expr } ->
        fprintf fmt "@{<magenta>scope@} %a" (Tuple.print print_expr)
          (Tuple.make [ expr ] [])
    | E_Fn { def = { compiled; on_compiled = _ }; _ } -> (
        match compiled with
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt
              "@{<magenta>fn@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
              (print_pattern_with_spans ~cache)
              arg print_expr body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Generic { def = { compiled; on_compiled = _ }; _ } -> (
        match compiled with
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt
              "@{<magenta>generic@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
              (print_pattern_with_spans ~cache)
              arg print_expr body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Tuple { tuple } ->
        fprintf fmt "tuple %a"
          (Tuple.print (fun fmt (~field_span:_, ~field_label:_, field_expr) ->
               print_expr fmt field_expr))
          tuple
    | E_Variant { label; label_span = _; value } ->
        fprintf fmt
          "@{<magenta>variant@} (@;<0 2>@[<v>label = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          Label.print label (Option.print print_expr) value
    | E_Apply { f; arg } ->
        fprintf fmt "@{<magenta>apply@} %a" (Tuple.print print_expr)
          (Tuple.make [] [ ("f", f); ("arg", arg) ])
    | E_InstantiateGeneric { generic; arg } ->
        fprintf fmt "@{<magenta>instantiate_generic@} %a"
          (Tuple.print print_expr)
          (Tuple.make [] [ ("generic", generic); ("arg", arg) ])
    | E_Assign { assignee; value } ->
        fprintf fmt
          "@{<magenta>assign@} (@;<0 2>@[<v>assignee = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          (print_assignee_expr_with_spans ~cache)
          assignee print_expr value
    | E_Ty expr ->
        fprintf fmt "@{<magenta>type@} %a" (print_ty_expr ~cache) expr
    | E_Native { expr } -> fprintf fmt "@{<magenta>native@} @{<green>%S@}" expr
    | E_Module { def } -> fprintf fmt "@{<magenta>module@} %a" print_expr def
    | E_Field { obj; field; field_span = _; label = _ } ->
        fprintf fmt
          "@{<magenta>field@} (@;<0 2>@[<v>obj = %a,@]@;<0 2>@[<v>field = %a@]@ )"
          print_expr obj String.print_maybe_escaped field
    | E_UseDotStar { used; bindings = _ } ->
        fprintf fmt "@{<magenta>use .*@} (@;<0 2>@[<v>used = %a,@]@ )"
          print_expr used
    | E_If { cond; then_case; else_case } ->
        fprintf fmt "@{<magenta>if@} %a" (Tuple.print print_expr)
          (Tuple.make []
             [
               ("cond", cond); ("then_case", then_case); ("else_case", else_case);
             ])
    | E_Match { value; branches } ->
        fprintf fmt
          "@{<magenta>match@} (@;<0 2>@[<v>value = %a,@]@;<0 2>@[<v>branches = %a@]@ )"
          print_expr value
          (List.print (print_expr_match_branch ~cache))
          branches
    | E_Error -> fprintf fmt "@{<red><error>@}"
    | E_Loop { body } ->
        fprintf fmt "@{<magenta>loop@} (@;<0 2>@[<v>body = %a,@]@ )" print_expr
          body
    | E_QuoteAst _ -> fprintf fmt "@{<magenta>quote_ast ...@}"
    | E_Unwindable { token; body } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          (print_pattern_with_spans ~cache)
          token print_expr body
    | E_Unwind { token; value } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          print_expr token print_expr value
    | E_InjectContext { context_ty; value } ->
        fprintf fmt
          "@{<magenta>inject_context@} (@;<0 2>@[<v>context_type = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          (print_context_type ~cache)
          context_ty print_expr value
    | E_CurrentContext { context_ty } ->
        fprintf fmt
          "@{<magenta>current_context@} (@;<0 2>@[<v>context_type = %a,@]@ )"
          (print_context_type ~cache)
          context_ty
    | E_TargetDependent { branches } ->
        fprintf fmt "@{<magenta>target dependent@} (";
        branches
        |> List.iter (fun branch ->
            fprintf fmt "@;<0 2>@[<v>%a@]"
              (print_target_dependent_branch ~cache)
              branch);
        fprintf fmt " )"

  and print_expr_match_branch :
      cache:RecurseCache.t -> formatter -> expr_match_branch -> unit =
   fun ~cache fmt { pattern; body } ->
    fprintf fmt "(@;<0 2>@[<v>pattern = %a,@]@;<0 2>@[<v>body = %a@]@ )"
      (print_pattern ~cache) pattern
      (print_expr_with_spans ~cache)
      body

  and print_context_type :
      cache:RecurseCache.t -> formatter -> value_context_ty -> unit =
   fun ~cache fmt { id; ty } ->
    fprintf fmt "<context %a of %a>" Id.print id (print_ty ~cache) ty

  and print_target_dependent_branch :
      cache:RecurseCache.t -> formatter -> expr_target_dependent_branch -> unit
      =
   fun ~cache fmt { cond; body } ->
    let print_expr = print_expr_with_spans in
    fprintf fmt "%a => %a" (print_expr ~cache) cond (print_expr ~cache) body

  and print_expr_with_spans : cache:RecurseCache.t -> formatter -> expr -> unit
      =
   fun ~cache fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}"
      (print_expr_shape ~cache (print_expr_with_spans ~cache))
      shape Span.print data.span

  and print_expr_with_types : cache:RecurseCache.t -> formatter -> expr -> unit
      =
   fun ~cache fmt { shape; data } ->
    fprintf fmt "%a @{<dim>:: %a@}"
      (print_expr_shape ~cache (print_expr_with_types ~cache))
      shape (print_ty ~cache) data.ty

  (* ASSIGNEE EXPR *)

  and print_assignee_expr_shape :
      cache:RecurseCache.t -> formatter -> assignee_expr_shape -> unit =
   fun ~cache fmt -> function
    | A_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | A_Unit -> fprintf fmt "()"
    | A_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" (print_binding ~cache) binding
    | A_Let pattern ->
        fprintf fmt "@{<magenta>let@} (@;<0 2>pattern = %a@ )"
          (print_pattern_with_spans ~cache)
          pattern
    | A_Error -> fprintf fmt "@{<red><error>@}"

  and print_assignee_expr_with_spans :
      cache:RecurseCache.t -> formatter -> assignee_expr -> unit =
   fun ~cache fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}"
      (print_assignee_expr_shape ~cache)
      shape Span.print data.span

  (* TYPE EXPR *)
  and print_ty_expr_shape :
      cache:RecurseCache.t -> formatter -> ty_expr_shape -> unit =
   fun ~cache fmt -> function
    | TE_Unit -> fprintf fmt "()"
    | TE_Fn { arg; result } ->
        fprintf fmt "@{<magenta>fn@} %a"
          (Tuple.print (print_ty_expr ~cache))
          (Tuple.make [] [ ("arg", arg); ("result", result) ])
    | TE_Expr expr ->
        fprintf fmt "@{<magenta>expr@} %a" (print_expr_with_spans ~cache) expr
    | TE_Tuple { tuple } ->
        fprintf fmt "@{<magenta>tuple@} %a"
          (Tuple.print (fun fmt (~field_span:_, ~field_label:_, field_expr) ->
               print_ty_expr ~cache fmt field_expr))
          tuple
    | TE_Union { elements } ->
        fprintf fmt "@{<magenta>union@} %a"
          (Tuple.print (print_ty_expr ~cache))
          (Tuple.make elements [])
    | TE_Variant { variants } ->
        fprintf fmt "@{<magenta>variant@} %a"
          (Tuple.print (Option.print (print_ty_expr ~cache)))
          (Tuple.make []
             (variants
             |> List.map (fun (~label_span:_, ~label, variant) ->
                 (Label.get_name label, variant))))
    | TE_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty_expr : cache:RecurseCache.t -> formatter -> ty_expr -> unit =
   fun ~cache fmt { compiled_shape; on_compiled = _; data } ->
    match compiled_shape with
    | Some shape ->
        fprintf fmt "%a @{<dim>at %a@}"
          (print_ty_expr_shape ~cache)
          shape Span.print data.span
    | None ->
        fprintf fmt "<not compiled yet> @{<dim>at %a@}" Span.print data.span

  (* PATTERN *)

  and print_pattern_shape :
      cache:RecurseCache.t -> formatter -> pattern_shape -> unit =
   fun ~cache fmt -> function
    | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | P_Unit -> fprintf fmt "()"
    | P_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" (print_binding ~cache) binding
    | P_Tuple { tuple } ->
        fprintf fmt "@{<magenta>tuple@} %a"
          (Tuple.print
             (fun fmt (~field_span:_, ~field_label:_, field_pattern) ->
               print_pattern_with_spans ~cache fmt field_pattern))
          tuple
    | P_Variant { label; label_span = _; value } ->
        fprintf fmt
          "@{<magenta>variant@} (@;<0 2>@[<v>label = %a,@]@;<0 2>@[<v>value = %a,@]@ )"
          Label.print label
          (Option.print (print_pattern_with_spans ~cache))
          value
    | P_Error -> fprintf fmt "@{<red><error>@}"

  and print_pattern_with_spans :
      cache:RecurseCache.t -> formatter -> pattern -> unit =
   fun ~cache fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}"
      (print_pattern_shape ~cache)
      shape Span.print data.span

  and print_pattern : cache:RecurseCache.t -> formatter -> pattern -> unit =
   fun ~cache fmt { shape; data = _ } ->
    match shape with
    | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | P_Unit -> fprintf fmt "()"
    | P_Binding binding -> print_binding ~cache fmt binding
    | P_Tuple { tuple } ->
        Tuple.print
          (fun fmt (~field_span:_, ~field_label:_, field_pattern) ->
            print_pattern ~cache fmt field_pattern)
          fmt tuple
    | P_Variant { label; label_span = _; value } ->
        fprintf fmt ":%a" Label.print label;
        value
        |> Option.iter (fun value ->
            fprintf fmt " %a" (print_pattern ~cache) value)
    | P_Error -> fprintf fmt "@{<red><error>@}"

  (* OTHER *)
  and print_binding : cache:RecurseCache.t -> formatter -> binding -> unit =
   fun ~cache fmt binding -> fprintf fmt "%a" Symbol.print binding.name

  and print_target : cache:RecurseCache.t -> formatter -> value_target -> unit =
   fun ~cache fmt { name } -> fprintf fmt "@{<italic><target=%S>@}" name
end

let with_cache f = fun fmt value -> f ~cache:(RecurseCache.create ()) fmt value
let print_ty = with_cache Impl.print_ty
let print_ty_shape = with_cache Impl.print_ty_shape
let print_value = with_cache Impl.print_value
let print_value_shape = with_cache Impl.print_value_shape
let print_expr_with_spans = with_cache Impl.print_expr_with_spans
let print_expr_with_types = with_cache Impl.print_expr_with_types
let print_expr_shape = with_cache Impl.print_expr_shape
let print_assignee_expr_shape = with_cache Impl.print_assignee_expr_shape

let print_assignee_expr_with_spans =
  with_cache Impl.print_assignee_expr_with_spans

let print_ty_expr = with_cache Impl.print_ty_expr
let print_ty_expr_shape = with_cache Impl.print_ty_expr_shape
let print_pattern_with_spans = with_cache Impl.print_pattern_with_spans
let print_pattern_shape = with_cache Impl.print_pattern_shape
let print_binding = with_cache Impl.print_binding
