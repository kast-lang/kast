open Std
open Kast_util
open Types

type options = {
  types : bool;
  spans : bool;
}

let rec print_scope_path fmt (scope : interpreter_scope) =
  (match scope.parent with
  | None -> ()
  | Some parent ->
      print_scope_path fmt parent;
      fprintf fmt ".");
  Id.print fmt scope.id

let print_var_scope fmt (scope : var_scope) =
  match scope with
  | None -> fprintf fmt "<root>"
  | Some scope ->
      Span.print_osc8 scope.span
        (fun fmt () -> fprintf fmt "<%a>" print_scope_path scope)
        () fmt

module RecurseCache = RecurseCache.Make (Id)

module Impl = struct
  let is_recursive = RecurseCache.create ()

  let rec _unused = ()

  and print_data ~options fmt (data : ir_data) =
    if options.spans then fprintf fmt " @{<dim>at %a@}" Span.print data.span;
    if options.types then fprintf fmt " @{<dim>:: %a@}" print_ty data.ty

  (* PLACE *)
  and print_place_ref : formatter -> place -> unit =
   fun fmt place ->
    (match place.state with
    | Uninitialized -> fprintf fmt "<uninitialized"
    | Occupied _ -> fprintf fmt "<occupied"
    | MovedOut -> fprintf fmt "<moved out");
    fprintf fmt " %a>" Id.print place.id

  and print_place_value : formatter -> place -> unit =
   fun fmt place ->
    match place.state with
    | Uninitialized -> fprintf fmt "<uninitialized>"
    | Occupied value -> print_value fmt value
    | MovedOut -> fprintf fmt "<moved out>"

  (* VALUE *)
  and print_value_shape : formatter -> value_shape -> unit =
   fun fmt -> function
    | V_Unit -> fprintf fmt "()"
    | V_Bool value -> fprintf fmt "%B" value
    | V_Ty ty ->
        fprintf fmt "type ";
        print_ty fmt ty
    | V_Int32 value -> fprintf fmt "@{<italic>%ld@}" value
    | V_Int64 value -> fprintf fmt "@{<italic>%Ld@}" value
    | V_Float64 value -> fprintf fmt "@{<italic>%f@}" value
    | V_Char value -> fprintf fmt "@{<green>%C@}" value
    | V_String value -> fprintf fmt "@{<green>%S@}" value
    | V_Ref { mut; place } ->
        fprintf fmt "&";
        if mut then fprintf fmt "mut ";
        print_place_ref fmt place
    | V_Tuple { ty = _; tuple } ->
        fprintf fmt "%a"
          (Tuple.print (fun fmt (field : value_tuple_field) ->
               print_place_value fmt field.place))
          tuple
    | V_Fn { ty; fn = _ } -> fprintf fmt "@{<italic><fn %a>@}" print_ty_fn ty
    | V_Variant { label; data; ty = _ } -> (
        fprintf fmt ":%a" Label.print label;
        match data with
        | Some data -> fprintf fmt " %a" print_place_value data
        | None -> ())
    | V_Generic g ->
        fprintf fmt "@{<italic><generic %a :: %a>@}" print_name_shape g.name
          print_ty_generic g.ty
    | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name
    | V_Ast ast -> fprintf fmt "%a" Ast.print ast
    | V_UnwindToken { id; result_ty = _ } ->
        fprintf fmt "<unwind %a>" Id.print id
    | V_ContextTy ty -> print_context_type fmt ty
    | V_Error -> fprintf fmt "@{<red><error>@}"
    | V_CompilerScope _ -> fprintf fmt "@{<italic><compiler scope>@}"
    | V_Opaque { ty = _; value = _ } -> fprintf fmt "@{<italic><opaque>@}"
    | V_Blocked blocked -> print_blocked_value fmt blocked
    | V_Target target -> print_target fmt target

  and print_value : formatter -> value -> unit =
   fun fmt { var; ty = _ } -> print_var print_value_shape fmt var

  and print_blocked_value : formatter -> blocked_value -> unit =
   fun fmt value -> print_blocked_value_shape fmt value.shape

  and print_blocked_value_shape : formatter -> blocked_value_shape -> unit =
   fun fmt value ->
    match value with
    | BV_Binding binding -> print_binding fmt binding
    | BV_Instantiate { generic; arg } ->
        fprintf fmt "%a[%a]" print_blocked_value generic print_value arg
    | BV_FieldRef { obj_ref; member } ->
        fprintf fmt "(&%a.%a)" print_blocked_value obj_ref Tuple.Member.print
          member
    | BV_ClaimRef ref -> fprintf fmt "%a^" print_blocked_value ref

  (* TY *)
  and print_ty_tuple : formatter -> ty_tuple -> unit =
   fun fmt { name; tuple } ->
    print_optionally_named fmt name (fun fmt ->
        fprintf fmt "%a"
          (Tuple.print
             ~options:
               { Tuple.default_print_options with named_field_middle = " :: " }
             (fun fmt (field : ty_tuple_field) -> print_ty fmt field.ty))
          tuple)

  and print_ty_variant : formatter -> ty_variant -> unit =
   fun fmt { name; variants } ->
    print_optionally_named fmt name (fun fmt ->
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
          variants;
        fprintf fmt "(row scope=%a)" print_var_scope
          (Inference.Var.scope variants.var))

  and print_ty_generic : formatter -> ty_generic -> unit =
   fun fmt { arg; result } ->
    fprintf fmt "[%a] %a"
      (print_pattern ~options:{ spans = false; types = true })
      arg print_ty result

  and print_ty_shape : formatter -> ty_shape -> unit =
   fun fmt -> function
    | T_Unit -> fprintf fmt "()"
    | T_Bool -> fprintf fmt "bool"
    | T_Int32 -> fprintf fmt "int32"
    | T_Int64 -> fprintf fmt "int64"
    | T_Float64 -> fprintf fmt "float64"
    | T_Char -> fprintf fmt "char"
    | T_String -> fprintf fmt "string"
    | T_Ref { mut; referenced } ->
        fprintf fmt "&%a%a" print_is_mutable mut print_ty referenced
    | T_Variant v -> print_ty_variant fmt v
    | T_Tuple t -> print_ty_tuple fmt t
    | T_Ty -> fprintf fmt "type"
    | T_Fn { arg; result } ->
        fprintf fmt "@[<hv>%a@] -> @[<hv>%a@]" print_ty arg print_ty result
    | T_Generic ty -> print_ty_generic fmt ty
    | T_Ast -> fprintf fmt "ast"
    | T_UnwindToken { result } -> fprintf fmt "<unwind %a>" print_ty result
    | T_Target -> fprintf fmt "target"
    | T_ContextTy -> fprintf fmt "context_type"
    | T_CompilerScope -> fprintf fmt "<compiler scope>"
    | T_Opaque { name } -> print_name fmt name
    | T_Blocked blocked -> print_blocked_value fmt blocked
    | T_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty : formatter -> ty -> unit =
   fun fmt { var } -> print_var print_ty_shape fmt var

  and print_is_mutable : formatter -> is_mutable -> unit =
   fun fmt { var } ->
    match var |> Inference.Var.inferred_opt with
    | Some true -> fprintf fmt "mut "
    | Some false -> ()
    | None -> fprintf fmt "?mut "

  and print_ty_fn : formatter -> ty_fn -> unit =
   fun fmt { arg; result } ->
    fprintf fmt "%a -> %a" print_ty arg print_ty result

  (* VAR *)
  and print_var : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit =
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
      (* fprintf fmt "@{<cyan>%a@}" print_var_scope (Inference.Var.scope var); *)
      cache |> RecurseCache.exit id)

  (* EXPR *)
  and print_expr_short : formatter -> expr -> unit =
   fun fmt expr ->
    let name =
      match expr.shape with
      | E_Constant _ -> "Constant"
      | E_Then _ -> "Then"
      | E_Ref _ -> "Ref"
      | E_Claim _ -> "ReadPlace"
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
      | E_Newtype _ -> "Newtype"
      | E_Native _ -> "Native"
      | E_Module _ -> "Module"
      | E_UseDotStar _ -> "UseDotStar"
      | E_If _ -> "If"
      | E_And _ -> "And"
      | E_Or _ -> "Or"
      | E_Match _ -> "Match"
      | E_Error -> "Error"
      | E_Loop _ -> "Loop"
      | E_QuoteAst _ -> "QuoteAst"
      | E_Unwindable _ -> "Unwindable"
      | E_Unwind _ -> "Unwind"
      | E_InjectContext _ -> "InjectContext"
      | E_CurrentContext _ -> "CurrentContext"
      | E_ImplCast _ -> "ImplCast"
      | E_Cast _ -> "Cast"
      | E_TargetDependent _ -> "TargetDependent"
    in
    fprintf fmt "%s" name

  and print_expr_shape : options:options -> formatter -> expr_shape -> unit =
   fun ~options fmt -> function
    | E_Ref { mut; place } ->
        fprintf fmt "@{<magenta>ref%s@} (@;<0 2>@[<v>place = %a,@]@ )"
          (if mut then " mut" else "")
          (print_place_expr ~options)
          place
    | E_Claim place ->
        fprintf fmt "@{<magenta>read_place@} (@;<0 2>@[<v>place = %a,@]@ )"
          (print_place_expr ~options)
          place
    | E_Constant value -> fprintf fmt "@{<magenta>const@} %a" print_value value
    | E_Then { list } ->
        fprintf fmt "@{<magenta>then@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make list [])
    | E_Stmt { expr } ->
        fprintf fmt "@{<magenta>stmt@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make [ expr ] [])
    | E_Scope { expr } ->
        fprintf fmt "@{<magenta>scope@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make [ expr ] [])
    | E_Fn { def = { compiled; on_compiled = _ }; _ } -> (
        match compiled with
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt
              "@{<magenta>fn@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
              (print_pattern ~options) arg (print_expr ~options) body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Generic { def = { compiled; on_compiled = _ }; _ } -> (
        match compiled with
        | Some { arg; body; evaled_result = _ } ->
            fprintf fmt
              "@{<magenta>generic@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
              (print_pattern ~options) arg (print_expr ~options) body
        | None -> fprintf fmt "@{<magenta>fn (not compiled)@}")
    | E_Tuple tuple -> print_ir_tuple '=' (print_expr ~options) fmt tuple
    | E_Variant { label; label_span = _; value } ->
        fprintf fmt
          "@{<magenta>variant@} (@;<0 2>@[<v>label = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          Label.print label
          (Option.print (print_expr ~options))
          value
    | E_Apply { f; arg } ->
        fprintf fmt "@{<magenta>apply@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make [] [ ("f", f); ("arg", arg) ])
    | E_InstantiateGeneric { generic; arg } ->
        fprintf fmt "@{<magenta>instantiate_generic@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make [] [ ("generic", generic); ("arg", arg) ])
    | E_Assign { assignee; value } ->
        fprintf fmt
          "@{<magenta>assign@} (@;<0 2>@[<v>assignee = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          (print_assignee_expr ~options)
          assignee
          (print_place_expr ~options)
          value
    | E_Ty expr ->
        fprintf fmt "@{<magenta>type@} %a" (print_ty_expr ~options) expr
    | E_Newtype e ->
        fprintf fmt "@{<magenta>newtype@} %a"
          (Tuple.print (print_ty_expr ~options))
          (Tuple.make [ e ] [])
    | E_Native { id = _; expr } ->
        fprintf fmt "@{<magenta>native@} @{<green>%S@}" expr
    | E_Module { def } ->
        fprintf fmt "@{<magenta>module@} %a" (print_expr ~options) def
    | E_UseDotStar { used; bindings = _ } ->
        fprintf fmt "@{<magenta>use .*@} (@;<0 2>@[<v>used = %a,@]@ )"
          (print_expr ~options) used
    | E_If { cond; then_case; else_case } ->
        fprintf fmt "@{<magenta>if@} %a"
          (Tuple.print (print_expr ~options))
          (Tuple.make []
             [
               ("cond", cond); ("then_case", then_case); ("else_case", else_case);
             ])
    | E_And { lhs; rhs } ->
        fprintf fmt
          "@{<magenta>and@} (@;<0 2>@[<v>lhs = %a,@]@;<0 2>@[<v>rhs = %a@]@ )"
          (print_expr ~options) lhs (print_expr ~options) rhs
    | E_Or { lhs; rhs } ->
        fprintf fmt
          "@{<magenta>or@} (@;<0 2>@[<v>lhs = %a,@]@;<0 2>@[<v>rhs = %a@]@ )"
          (print_expr ~options) lhs (print_expr ~options) rhs
    | E_Match { value; branches } ->
        fprintf fmt
          "@{<magenta>match@} (@;<0 2>@[<v>value = %a,@]@;<0 2>@[<v>branches = %a@]@ )"
          (print_place_expr ~options)
          value
          (List.print (print_expr_match_branch ~options))
          branches
    | E_Error -> fprintf fmt "@{<red><error>@}"
    | E_Loop { body } ->
        fprintf fmt "@{<magenta>loop@} (@;<0 2>@[<v>body = %a,@]@ )"
          (print_expr ~options) body
    | E_QuoteAst _ -> fprintf fmt "@{<magenta>quote_ast ...@}"
    | E_Unwindable { token; body } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          (print_pattern ~options) token (print_expr ~options) body
    | E_Unwind { token; value } ->
        fprintf fmt
          "@{<magenta>unwindable@} (@;<0 2>@[<v>token = %a,@]@;<0 2>@[<v>body = %a@]@ )"
          (print_expr ~options) token (print_expr ~options) value
    | E_InjectContext { context_ty; value } ->
        fprintf fmt
          "@{<magenta>inject_context@} (@;<0 2>@[<v>context_type = %a,@]@;<0 2>@[<v>value = %a@]@ )"
          print_context_type context_ty (print_expr ~options) value
    | E_CurrentContext { context_ty } ->
        fprintf fmt
          "@{<magenta>current_context@} (@;<0 2>@[<v>context_type = %a,@]@ )"
          print_context_type context_ty
    | E_ImplCast { value; target; impl } ->
        fprintf fmt
          "@{<magenta>cast@} (@;<0 2>@[<v>value = %a,@]@;<0 2>@[<v>target = %a@]@;<0 2>@[<v>impl = %a@]@ )"
          (print_expr ~options) value print_value target (print_expr ~options)
          impl
    | E_Cast { value; target } ->
        fprintf fmt
          "@{<magenta>cast@} (@;<0 2>@[<v>value = %a,@]@;<0 2>@[<v>target = %a@]@ )"
          (print_expr ~options) value print_value target
    | E_TargetDependent { branches; interpreter_branch = _ } ->
        fprintf fmt "@{<magenta>target dependent@} (";
        branches
        |> List.iter (fun branch ->
            fprintf fmt "@;<0 2>@[<v>%a@]"
              (print_target_dependent_branch ~options)
              branch);
        fprintf fmt " )"

  and print_ir_tuple :
      'a. char -> (formatter -> 'a -> unit) -> formatter -> 'a tuple_of -> unit
      =
   fun eq print_a fmt { parts } ->
    fprintf fmt "tuple %a" (List.print (print_ir_tuple_part eq print_a)) parts

  and print_ir_tuple_part :
      'a.
      char -> (formatter -> 'a -> unit) -> formatter -> 'a tuple_part_of -> unit
      =
   fun eq print_a fmt -> function
    | Field field -> print_ir_tuple_field eq print_a fmt field
    | Unpack a -> fprintf fmt "...%a" print_a a

  and print_ir_tuple_field :
      'a.
      char ->
      (formatter -> 'a -> unit) ->
      formatter ->
      'a tuple_field_of ->
      unit =
   fun eq print_a fmt { label; label_span = _; field } ->
    match label with
    | Some label -> fprintf fmt "%a %c %a" Label.print label eq print_a field
    | None -> print_a fmt field

  and print_expr_match_branch :
      options:options -> formatter -> expr_match_branch -> unit =
   fun ~options fmt { pattern; body } ->
    fprintf fmt "(@;<0 2>@[<v>pattern = %a,@]@;<0 2>@[<v>body = %a@]@ )"
      (print_pattern ~options) pattern (print_expr ~options) body

  and print_context_type : formatter -> value_context_ty -> unit =
   fun fmt { id; ty } ->
    fprintf fmt "<context %a of %a>" Id.print id print_ty ty

  and print_target_dependent_branch :
      options:options -> formatter -> expr_target_dependent_branch -> unit =
   fun ~options fmt { cond; body } ->
    fprintf fmt "%a => %a" (print_expr ~options) cond (print_expr ~options) body

  and print_expr : options:options -> formatter -> expr -> unit =
   fun ~options fmt { shape; data } ->
    print_expr_shape ~options fmt shape;
    print_data ~options fmt data

  (* PLACE EXPR *)
  and print_place_expr_shape :
      options:options -> formatter -> place_expr_shape -> unit =
   fun ~options fmt -> function
    | PE_Deref ref ->
        fprintf fmt "@{<magenta>deref@} (@;<0 2>@[<v>ref = %a,@]@ )"
          (print_expr ~options) ref
    | PE_Temp expr ->
        fprintf fmt "@{<magenta>temp@} (@;<0 2>@[<v>expr = %a,@]@ )"
          (print_expr ~options) expr
    | PE_Binding binding ->
        fprintf fmt "@{<magenta>binding@} %a" print_binding binding
    | PE_Field { obj; field; field_span = _ } ->
        fprintf fmt
          "@{<magenta>field@} (@;<0 2>@[<v>obj = %a,@]@;<0 2>@[<v>field = %a@]@ )"
          (print_place_expr ~options)
          obj
          (print_field_expr ~options)
          field
    | PE_Error -> fprintf fmt "@{<red><error>@}"

  and print_field_expr ~options fmt (field : field_expr) =
    match field with
    | Index i -> fprintf fmt "%d" i
    | Name s -> String.print_maybe_escaped fmt (Label.get_name s)
    | Expr e -> fprintf fmt "(%a)" (print_expr ~options) e

  and print_place_expr : options:options -> formatter -> place_expr -> unit =
   fun ~options fmt { shape; mut = _; data } ->
    print_place_expr_shape ~options fmt shape;
    print_data ~options fmt data

  (* ASSIGNEE EXPR *)

  and print_assignee_expr_shape :
      options:options -> formatter -> assignee_expr_shape -> unit =
   fun ~options fmt -> function
    | A_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | A_Unit -> fprintf fmt "()"
    | A_Place place ->
        fprintf fmt "@{<magenta>place@} (@;<0 2>place = %a@ )"
          (print_place_expr ~options)
          place
    | A_Tuple tuple ->
        print_ir_tuple '=' (print_assignee_expr ~options) fmt tuple
    | A_Let pattern ->
        fprintf fmt "@{<magenta>let@} (@;<0 2>pattern = %a@ )"
          (print_pattern ~options) pattern
    | A_Error -> fprintf fmt "@{<red><error>@}"

  and print_assignee_expr :
      options:options -> formatter -> assignee_expr -> unit =
   fun ~options fmt { shape; data } ->
    print_assignee_expr_shape ~options fmt shape;
    print_data ~options fmt data

  (* TYPE EXPR *)

  and print_ty_expr_shape :
      options:options -> formatter -> ty_expr_shape -> unit =
   fun ~options fmt -> function
    | TE_Unit -> fprintf fmt "()"
    | TE_Ref { mut; referenced } ->
        fprintf fmt "@{<magenta>ref%a@} %a" print_is_mutable mut
          (Tuple.print (print_ty_expr ~options))
          (Tuple.make [ referenced ] [])
    | TE_Fn { arg; result } ->
        fprintf fmt "@{<magenta>fn@} %a"
          (Tuple.print (print_ty_expr ~options))
          (Tuple.make [] [ ("arg", arg); ("result", result) ])
    | TE_Expr expr ->
        fprintf fmt "@{<magenta>expr@} %a" (print_expr ~options) expr
    | TE_Tuple tuple -> print_ir_tuple '=' (print_ty_expr ~options) fmt tuple
    | TE_Union { elements } ->
        fprintf fmt "@{<magenta>union@} %a"
          (Tuple.print (print_ty_expr ~options))
          (Tuple.make elements [])
    | TE_Variant { variants } ->
        fprintf fmt "@{<magenta>variant@} %a"
          (Tuple.print (Option.print (print_ty_expr ~options)))
          (Tuple.make []
             (variants
             |> List.map
                  (fun
                    ({ label_span = _; label; value } : ty_expr_variant_variant)
                  -> (Label.get_name label, value))))
    | TE_Error -> fprintf fmt "@{<red><error>@}"

  and print_ty_expr : options:options -> formatter -> ty_expr -> unit =
   fun ~options fmt { compiled_shape; on_compiled = _; data } ->
    (match compiled_shape with
    | Some shape -> print_ty_expr_shape ~options fmt shape
    | None -> fprintf fmt "<not compiled yet>");
    print_data ~options fmt data

  (* PATTERN *)
  and print_pattern_shape :
      options:options -> formatter -> pattern_shape -> unit =
   fun ~options fmt -> function
    | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
    | P_Unit -> fprintf fmt "()"
    | P_Ref inner ->
        fprintf fmt "@{<magenta>&@}%a"
          (Tuple.print (print_pattern ~options))
          (Tuple.make [ inner ] [])
    | P_Binding { by_ref; binding } ->
        if by_ref then fprintf fmt "@{<magenta>ref@} ";
        print_binding fmt binding
    | P_Tuple tuple -> print_ir_tuple '=' (print_pattern ~options) fmt tuple
    | P_Variant { label; label_span = _; value } ->
        fprintf fmt ":%a %a" Label.print label
          (Option.print (print_pattern ~options))
          value
    | P_Error -> fprintf fmt "@{<red><error>@}"

  and print_pattern : options:options -> formatter -> pattern -> unit =
   fun ~options fmt { shape; data } ->
    print_pattern_shape ~options fmt shape;
    print_data ~options fmt data

  (* OTHER *)
  and print_binding : formatter -> binding -> unit =
   fun fmt binding ->
    let span = binding.label |> Label.get_span in
    Span.print_osc8 span Symbol.print binding.name fmt;
    Id.print fmt binding.id

  and print_target : formatter -> value_target -> unit =
   fun fmt { name } -> fprintf fmt "@{<italic><target=%S>@}" name

  and print_optionally_named :
      formatter -> optional_name -> (formatter -> unit) -> unit =
   fun fmt name f ->
    match name.var |> Inference.Var.inferred_opt with
    | Some (Some name) ->
        print_name_shape fmt name;
        fprintf fmt "(=%t)" f
    | _ -> f fmt

  and print_name : formatter -> name -> unit =
   fun fmt { var } -> print_var print_name_shape fmt var

  and print_name_part : formatter -> name_part -> unit =
   fun fmt -> print_name_shape_part fmt

  and print_name_shape_part : formatter -> name_part -> unit =
   fun fmt -> function
    | Uri uri ->
        let name =
          Uri.path uri |> Filename.basename |> Filename.remove_extension
        in
        fprintf fmt "%s" name
    | Str s -> fprintf fmt "%s" s
    | Symbol symbol -> Symbol.print fmt symbol

  and print_name_shape : formatter -> name_shape -> unit =
   fun fmt shape ->
    match shape with
    | Simple part -> print_name_shape_part fmt part
    | Concat (a, b) -> fprintf fmt "%a.%a" print_name_shape a print_name_part b
    | Instantiation { generic; arg } ->
        fprintf fmt "%a[%a]" print_value generic print_value arg
end

let with_cache f =
 fun fmt value ->
  RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
      f Format.noop_formatter value);
  RecurseCache.with_cache (RecurseCache.create ()) (fun () -> f fmt value);
  Impl.is_recursive |> RecurseCache.clear

let print_ty = with_cache Impl.print_ty
let print_ty_shape = with_cache Impl.print_ty_shape
let print_blocked_value = with_cache Impl.print_blocked_value
let print_blocked_value_shape = with_cache Impl.print_blocked_value_shape
let print_value = with_cache Impl.print_value
let print_value_shape = with_cache Impl.print_value_shape

let print_expr_with_spans =
  with_cache (Impl.print_expr ~options:{ spans = true; types = false })

let print_expr_with_types =
  with_cache (Impl.print_expr ~options:{ spans = false; types = true })

let print_expr_short = with_cache Impl.print_expr_short

let print_assignee_expr =
  with_cache (Impl.print_assignee_expr ~options:{ spans = true; types = false })

let print_ty_expr =
  with_cache (Impl.print_ty_expr ~options:{ spans = true; types = false })

let print_place_expr =
  with_cache (Impl.print_place_expr ~options:{ spans = true; types = false })

let print_place_expr_shape =
  with_cache
    (Impl.print_place_expr_shape ~options:{ spans = true; types = false })

let print_expr_shape ~options = with_cache (Impl.print_expr_shape ~options)

let print_assignee_expr ~options =
  with_cache (Impl.print_assignee_expr ~options)

let print_assignee_expr_shape ~options =
  with_cache (Impl.print_assignee_expr_shape ~options)

let print_ty_expr ~options = with_cache (Impl.print_ty_expr ~options)

let print_ty_expr_shape ~options =
  with_cache (Impl.print_ty_expr_shape ~options)

let print_pattern ~options = with_cache (Impl.print_pattern ~options)

let print_pattern_shape ~options =
  with_cache (Impl.print_pattern_shape ~options)

let print_binding = with_cache Impl.print_binding
let print_place_ref = with_cache Impl.print_place_ref
let print_place_value = with_cache Impl.print_place_value
let print_name = with_cache Impl.print_name
let print_name_shape = with_cache Impl.print_name_shape
let print_name_part = with_cache Impl.print_name_part
let print_ty_tuple = with_cache Impl.print_ty_tuple
let print_ty_variant = with_cache Impl.print_ty_variant
let print_ty_generic = with_cache Impl.print_ty_generic
