open Std
open Kast_util
open Types

module Impl = struct
  let is_recursive = RecurseCache.create ()

  let rec _unused = ()

  (* VALUE *)
  and print_value_shape : formatter -> value_shape -> unit =
   fun fmt -> function
    | V_Unit -> fprintf fmt "()"
    | V_Bool value -> fprintf fmt "%B" value
    | V_Ty ty ->
        fprintf fmt "type ";
        print_ty fmt ty
    | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
    | V_Char value -> fprintf fmt "@{<green>%C@}" value
    | V_String value -> fprintf fmt "@{<green>%S@}" value
    | V_Tuple { tuple } ->
        fprintf fmt "%a"
          (Tuple.print (fun fmt (field : value_tuple_field) ->
               print_value fmt field.value))
          tuple
    | V_Fn _ -> fprintf fmt "@{<italic><fn>@}"
    | V_Variant { label; data; ty = _ } -> (
        fprintf fmt ":%a" Label.print label;
        match data with
        | Some data -> fprintf fmt " %a" print_value data
        | None -> ())
    | V_Generic _ -> fprintf fmt "@{<italic><generic>@}"
    | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name
    | V_Ast ast -> fprintf fmt "%a" Ast.print ast
    | V_UnwindToken { id; result_ty = _ } ->
        fprintf fmt "<unwind %a>" Id.print id
    | V_ContextTy ty -> print_context_type fmt ty
    | V_Error -> fprintf fmt "@{<red><error>@}"
    | V_Binding binding -> fprintf fmt "<binding %a>" print_binding binding
    | V_CompilerScope _ -> fprintf fmt "@{<italic><compiler scope>@}"
    | V_Target target -> print_target fmt target

  and print_value : formatter -> value -> unit =
   fun fmt { var; ty = _ } -> print_var print_value_shape fmt var

  (* TY *)
  and print_ty_shape : formatter -> ty_shape -> unit =
   fun fmt -> function
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
               | Some data -> fprintf fmt " %a" print_ty data
               | None -> ()))
          variants
    | T_Tuple { tuple } ->
        fprintf fmt "%a"
          (Tuple.print
             ~options:
               { Tuple.default_print_options with named_field_middle = " :: " }
             (fun fmt (field : ty_tuple_field) -> print_ty fmt field.ty))
          tuple
    | T_Ty -> fprintf fmt "type"
    | T_Fn { arg; result } ->
        fprintf fmt "@[<hv>%a@] -> @[<hv>%a@]" print_ty arg print_ty result
    | T_Generic { def = { compiled; on_compiled = _ } } -> (
        match compiled with
        | None -> fprintf fmt "@{<italic><generic>@}"
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt "[%a] %a" print_pattern arg print_ty body.data.ty)
    | T_Ast -> fprintf fmt "ast"
    | T_UnwindToken { result } -> fprintf fmt "<unwind %a>" print_ty result
    | T_Target -> fprintf fmt "target"
    | T_ContextTy -> fprintf fmt "context_type"
    | T_CompilerScope -> fprintf fmt "<compiler scope>"
    | T_Binding binding -> fprintf fmt "%a" print_binding binding
    | T_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty : formatter -> ty -> unit =
   fun fmt { var } -> print_var print_ty_shape fmt var

  (* VAR *)
  and print_var :
      'a. (formatter -> 'a -> unit) -> formatter -> 'a Inference.Var.t -> unit =
   fun print_shape fmt var ->
    let id = var |> Inference.Var.recurse_id in
    let cache = RecurseCache.get () in
    if cache |> RecurseCache.depth id > 0 then (
      is_recursive |> RecurseCache.enter id;
      fprintf fmt "<recursive %a>" Id.print id)
    else (
      cache |> RecurseCache.enter id;
      if is_recursive |> RecurseCache.is_visited id then
        fprintf fmt "<id=%a>" Id.print id;
      Inference.Var.print print_shape fmt var;
      cache |> RecurseCache.exit id)

  (* EXPR *)
  and print_expr_short : formatter -> expr -> unit =
   fun fmt expr ->
    let name =
      match expr.shape with
      | E_Constant _ -> "Constant"
      | E_Binding _ -> "Binding"
      | E_Then _ -> "Then"
      | E_Stmt _ -> "Stmt"
      | E_Scope _ -> "Scope"
      | E_Fn _ -> "Fn"
      | E_Generic _ -> "Generic"
      | E_Tuple _ -> "Tuple"
      | E_Variant _ -> "Variant"
      | E_Apply _ -> "Apply"
      | E_InstantiateGeneric _ -> "InstantiateGeneric"
      | E_Assign _ -> "Assign"
      | E_Ty _ -> "Ty"
      | E_Native _ -> "Native"
      | E_Module _ -> "Module"
      | E_Field _ -> "Field"
      | E_UseDotStar _ -> "UseDotStar"
      | E_If _ -> "If"
      | E_Match _ -> "Match"
      | E_Error -> "Error"
      | E_Loop _ -> "Loop"
      | E_QuoteAst _ -> "QuoteAst"
      | E_Unwindable _ -> "Unwindable"
      | E_Unwind _ -> "Unwind"
      | E_InjectContext _ -> "InjectContext"
      | E_CurrentContext _ -> "CurrentContext"
      | E_TargetDependent _ -> "TargetDependent"
    in
    fprintf fmt "%s" name

  and print_expr_shape :
      (formatter -> expr -> unit) -> formatter -> expr_shape -> unit =
   fun print_expr fmt -> function
    | E_Constant value -> fprintf fmt "@{<magenta>const@} %a" print_value value
    | E_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" print_binding binding
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
              print_pattern_with_spans arg print_expr body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Generic { def = { compiled; on_compiled = _ }; _ } -> (
        match compiled with
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt
              "@{<magenta>generic@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
              print_pattern_with_spans arg print_expr body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Tuple { tuple } ->
        fprintf fmt "tuple %a"
          (Tuple.print (fun fmt { label_span = _; label = _; field } ->
               print_expr fmt field))
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
          print_assignee_expr_with_spans assignee print_expr value
    | E_Ty expr -> fprintf fmt "@{<magenta>type@} %a" print_ty_expr expr
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
          (List.print print_expr_match_branch)
          branches
    | E_Error -> fprintf fmt "@{<red><error>@}"
    | E_Loop { body } ->
        fprintf fmt "@{<magenta>loop@} (@;<0 2>@[<v>body = %a,@]@ )" print_expr
          body
    | E_QuoteAst _ -> fprintf fmt "@{<magenta>quote_ast ...@}"
    | E_Unwindable { token; body } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          print_pattern_with_spans token print_expr body
    | E_Unwind { token; value } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          print_expr token print_expr value
    | E_InjectContext { context_ty; value } ->
        fprintf fmt
          "@{<magenta>inject_context@} (@;<0 2>@[<v>context_type = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          print_context_type context_ty print_expr value
    | E_CurrentContext { context_ty } ->
        fprintf fmt
          "@{<magenta>current_context@} (@;<0 2>@[<v>context_type = %a,@]@ )"
          print_context_type context_ty
    | E_TargetDependent { branches } ->
        fprintf fmt "@{<magenta>target dependent@} (";
        branches
        |> List.iter (fun branch ->
            fprintf fmt "@;<0 2>@[<v>%a@]" print_target_dependent_branch branch);
        fprintf fmt " )"

  and print_expr_match_branch : formatter -> expr_match_branch -> unit =
   fun fmt { pattern; body } ->
    fprintf fmt "(@;<0 2>@[<v>pattern = %a,@]@;<0 2>@[<v>body = %a@]@ )"
      print_pattern pattern print_expr_with_spans body

  and print_context_type : formatter -> value_context_ty -> unit =
   fun fmt { id; ty } ->
    fprintf fmt "<context %a of %a>" Id.print id print_ty ty

  and print_target_dependent_branch :
      formatter -> expr_target_dependent_branch -> unit =
   fun fmt { cond; body } ->
    let print_expr = print_expr_with_spans in
    fprintf fmt "%a => %a" print_expr cond print_expr body

  and print_expr_with_spans : formatter -> expr -> unit =
   fun fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}"
      (print_expr_shape print_expr_with_spans)
      shape Span.print data.span

  and print_expr_with_types : formatter -> expr -> unit =
   fun fmt { shape; data } ->
    fprintf fmt "%a @{<dim>:: %a@}"
      (print_expr_shape print_expr_with_types)
      shape print_ty data.ty

  (* ASSIGNEE EXPR *)

  and print_assignee_expr_shape : formatter -> assignee_expr_shape -> unit =
   fun fmt -> function
    | A_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | A_Unit -> fprintf fmt "()"
    | A_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" print_binding binding
    | A_Let pattern ->
        fprintf fmt "@{<magenta>let@} (@;<0 2>pattern = %a@ )"
          print_pattern_with_spans pattern
    | A_Error -> fprintf fmt "@{<red><error>@}"

  and print_assignee_expr_with_spans : formatter -> assignee_expr -> unit =
   fun fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}" print_assignee_expr_shape shape Span.print
      data.span

  (* TYPE EXPR *)

  and print_ty_expr_shape : formatter -> ty_expr_shape -> unit =
   fun fmt -> function
    | TE_Unit -> fprintf fmt "()"
    | TE_Fn { arg; result } ->
        fprintf fmt "@{<magenta>fn@} %a"
          (Tuple.print print_ty_expr)
          (Tuple.make [] [ ("arg", arg); ("result", result) ])
    | TE_Expr expr ->
        fprintf fmt "@{<magenta>expr@} %a" print_expr_with_spans expr
    | TE_Tuple { tuple } ->
        fprintf fmt "@{<magenta>tuple@} %a"
          (Tuple.print (fun fmt { label_span = _; label = _; field } ->
               print_ty_expr fmt field))
          tuple
    | TE_Union { elements } ->
        fprintf fmt "@{<magenta>union@} %a"
          (Tuple.print print_ty_expr)
          (Tuple.make elements [])
    | TE_Variant { variants } ->
        fprintf fmt "@{<magenta>variant@} %a"
          (Tuple.print (Option.print print_ty_expr))
          (Tuple.make []
             (variants
             |> List.map
                  (fun
                    ({ label_span = _; label; value } : ty_expr_variant_variant)
                  -> (Label.get_name label, value))))
    | TE_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty_expr : formatter -> ty_expr -> unit =
   fun fmt { compiled_shape; on_compiled = _; data } ->
    match compiled_shape with
    | Some shape ->
        fprintf fmt "%a @{<dim>at %a@}" print_ty_expr_shape shape Span.print
          data.span
    | None ->
        fprintf fmt "<not compiled yet> @{<dim>at %a@}" Span.print data.span

  (* PATTERN *)

  and print_pattern_shape : formatter -> pattern_shape -> unit =
   fun fmt -> function
    | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | P_Unit -> fprintf fmt "()"
    | P_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" print_binding binding
    | P_Tuple { tuple } ->
        fprintf fmt "@{<magenta>tuple@} %a"
          (Tuple.print (fun fmt { label_span = _; label = _; field } ->
               print_pattern_with_spans fmt field))
          tuple
    | P_Variant { label; label_span = _; value } ->
        fprintf fmt
          "@{<magenta>variant@} (@;<0 2>@[<v>label = %a,@]@;<0 2>@[<v>value = %a,@]@ )"
          Label.print label
          (Option.print print_pattern_with_spans)
          value
    | P_Error -> fprintf fmt "@{<red><error>@}"

  and print_pattern_with_spans : formatter -> pattern -> unit =
   fun fmt { shape; data } ->
    fprintf fmt "%a @{<dim>at %a@}" print_pattern_shape shape Span.print
      data.span

  and print_pattern : formatter -> pattern -> unit =
   fun fmt { shape; data = _ } ->
    match shape with
    | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | P_Unit -> fprintf fmt "()"
    | P_Binding binding -> print_binding fmt binding
    | P_Tuple { tuple } ->
        Tuple.print
          (fun fmt { label_span = _; label = _; field } ->
            print_pattern fmt field)
          fmt tuple
    | P_Variant { label; label_span = _; value } ->
        fprintf fmt ":%a" Label.print label;
        value
        |> Option.iter (fun value -> fprintf fmt " %a" print_pattern value)
    | P_Error -> fprintf fmt "@{<red><error>@}"

  (* OTHER *)
  and print_binding : formatter -> binding -> unit =
   fun fmt binding -> fprintf fmt "%a" Symbol.print binding.name

  and print_target : formatter -> value_target -> unit =
   fun fmt { name } -> fprintf fmt "@{<italic><target=%S>@}" name
end

let with_cache f =
 fun fmt value ->
  RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
      f Format.noop_formatter value);
  RecurseCache.with_cache (RecurseCache.create ()) (fun () -> f fmt value);
  Impl.is_recursive |> RecurseCache.clear

let print_ty = with_cache Impl.print_ty
let print_ty_shape = with_cache Impl.print_ty_shape
let print_value = with_cache Impl.print_value
let print_value_shape = with_cache Impl.print_value_shape
let print_expr_with_spans = with_cache Impl.print_expr_with_spans
let print_expr_with_types = with_cache Impl.print_expr_with_types
let print_expr_short = with_cache Impl.print_expr_short
let print_expr_shape print_expr = with_cache (Impl.print_expr_shape print_expr)
let print_assignee_expr_shape = with_cache Impl.print_assignee_expr_shape

let print_assignee_expr_with_spans =
  with_cache Impl.print_assignee_expr_with_spans

let print_ty_expr = with_cache Impl.print_ty_expr
let print_ty_expr_shape = with_cache Impl.print_ty_expr_shape
let print_pattern_with_spans = with_cache Impl.print_pattern_with_spans
let print_pattern_shape = with_cache Impl.print_pattern_shape
let print_binding = with_cache Impl.print_binding
