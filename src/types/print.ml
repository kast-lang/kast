open Std
open Kast_util
open Types

let rec _unused = ()

(* VALUE *)
and print_value_shape : formatter -> value_shape -> unit =
 fun fmt -> function
  | V_Unit -> fprintf fmt "()"
  | V_Bool value -> fprintf fmt "%B" value
  | V_Ty ty -> print_ty fmt ty
  | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
  | V_String value -> fprintf fmt "@{<green>%S@}" value
  | V_Tuple { tuple } -> fprintf fmt "%a" (Tuple.print print_value) tuple
  | V_Fn f -> fprintf fmt "@{<italic><fn>@}"
  | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name
  | V_Ast ast -> fprintf fmt "%a" Ast.print ast
  | V_Error -> fprintf fmt "@{<red><error>@}"

and print_value : formatter -> value -> unit =
 fun fmt { shape } -> print_value_shape fmt shape

(* TY *)
and print_ty_shape : formatter -> ty_shape -> unit =
 fun fmt -> function
  | T_Unit -> fprintf fmt "()"
  | T_Bool -> fprintf fmt "bool"
  | T_Int32 -> fprintf fmt "int32"
  | T_String -> fprintf fmt "string"
  | T_Tuple { tuple } -> fprintf fmt "%a" (Tuple.print print_ty) tuple
  | T_Ty -> fprintf fmt "type"
  | T_Fn { arg; result } -> fprintf fmt "%a -> %a" print_ty arg print_ty result
  | T_Ast -> fprintf fmt "ast"
  | T_Error -> fprintf fmt "@{<red><error>@}"

and print_ty : formatter -> ty -> unit =
 fun fmt { var } -> Inference.Var.print print_ty_shape fmt var

(* EXPR *)
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
  | E_Fn { arg; body; evaled_result = _ } ->
      fprintf fmt
        "@{<magenta>fn@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
        print_pattern_with_spans arg print_expr body
  | E_Tuple { tuple } -> fprintf fmt "tuple %a" (Tuple.print print_expr) tuple
  | E_Apply { f; arg } ->
      fprintf fmt "@{<magenta>apply@} %a" (Tuple.print print_expr)
        (Tuple.make [] [ ("f", f); ("arg", arg) ])
  | E_Assign { assignee; value } ->
      fprintf fmt
        "@{<magenta>assign@} (@;<0 2>@[<v>assignee = %a,@]@;<0 2>@[<v>value = %a@]@ )"
        print_assignee_expr_with_spans assignee print_expr value
  | E_Ty expr -> fprintf fmt "@{<magenta>type@} %a" print_ty_expr expr
  | E_Native { expr } -> fprintf fmt "@{<magenta>native@} @{<green>%S@}" expr
  | E_Module { def } -> fprintf fmt "@{<magenta>module@} %a" print_expr def
  | E_Field { obj; field } ->
      fprintf fmt
        "@{<magenta>field@} (@;<0 2>@[<v>obj = %a,@]@;<0 2>@[<v>field = %a@]@ )"
        print_expr obj String.print_maybe_escaped field
  | E_UseDotStar { used; bindings = _ } ->
      fprintf fmt "@{<magenta>use .*@} (@;<0 2>@[<v>used = %a,@]@ )" print_expr
        used
  | E_If { cond; then_case; else_case } ->
      fprintf fmt "@{<magenta>if@} %a" (Tuple.print print_expr)
        (Tuple.make []
           [
             ("cond", cond); ("then_case", then_case); ("else_case", else_case);
           ])
  | E_Error -> fprintf fmt "@{<red><error>@}"
  | E_Loop { body } ->
      fprintf fmt "@{<magenta>loop@} (@;<0 2>@[<v>body = %a,@]@ )" print_expr
        body
  | E_QuoteAst expr -> fprintf fmt "@{<magenta>quote_ast ...@}"

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
      fprintf fmt "@{<magenta>tuple@} %a" (Tuple.print print_ty_expr) tuple
  | TE_Error -> fprintf fmt "@{<red><error>@}"

and print_ty_expr : formatter -> ty_expr -> unit =
 fun fmt { shape; data } ->
  fprintf fmt "%a @{<dim>at %a@}" print_ty_expr_shape shape Span.print data.span

(* PATTERN *)

and print_pattern_shape : formatter -> pattern_shape -> unit =
 fun fmt -> function
  | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
  | P_Unit -> fprintf fmt "()"
  | P_Binding binding ->
      fprintf fmt "@{<magenta>binding@} %a" print_binding binding
  | P_Tuple { tuple } ->
      fprintf fmt "@{<magenta>tuple@} %a"
        (Tuple.print print_pattern_with_spans)
        tuple
  | P_Error -> fprintf fmt "@{<red><error>@}"

and print_pattern_with_spans : formatter -> pattern -> unit =
 fun fmt { shape; data } ->
  fprintf fmt "%a @{<dim>at %a@}" print_pattern_shape shape Span.print data.span

(* OTHER *)
and print_binding : formatter -> binding -> unit =
 fun fmt binding -> fprintf fmt "%a" Symbol.print binding.name
