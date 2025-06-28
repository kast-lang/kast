open Std
open Kast_util
open Types

let rec _unused = ()

(* VALUE *)
and print_value_shape : formatter -> value_shape -> unit =
 fun fmt -> function
  | V_Unit -> fprintf fmt "()"
  | V_Ty ty -> print_ty fmt ty
  | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
  | V_String value -> fprintf fmt "@{<green>%S@}" value
  | V_Tuple { tuple } -> fprintf fmt "%a" (Tuple.print print_value) tuple
  | V_Fn f -> fprintf fmt "@{<italic><fn>@}"
  | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name

and print_value : formatter -> value -> unit =
 fun fmt value -> print_value_shape fmt value.shape

(* TY *)
and print_ty_shape : formatter -> ty_shape -> unit =
 fun fmt -> function
  | T_Ty -> fprintf fmt "type"
  | T_Int32 -> fprintf fmt "int32"

and print_ty : formatter -> ty -> unit =
 fun fmt ty -> print_ty_shape fmt ty.shape

(* EXPR *)
and print_expr_shape : formatter -> expr_shape -> unit =
 fun fmt -> function
  | E_Constant value -> fprintf fmt "@{<magenta>const@} %a" print_value value
  | E_Binding binding ->
      fprintf fmt "@{<magenta>binding@} %a" print_binding binding
  | E_Then { a; b } ->
      fprintf fmt "@{<magenta>then@} %a" (Tuple.print print_expr)
        (Tuple.make [ a; b ] [])
  | E_Scope { expr } ->
      fprintf fmt "@{<magenta>scope@} %a" (Tuple.print print_expr)
        (Tuple.make [ expr ] [])
  | E_Fn { arg; body } ->
      fprintf fmt
        "@{<magenta>fn@} (@;<0 2>@[<v>arg = %a,@]@;<0 2>@[<v>body = %a@]@ )"
        print_pattern arg print_expr body
  | E_Tuple { tuple } -> fprintf fmt "tuple %a" (Tuple.print print_expr) tuple
  | E_Apply { f; arg } ->
      fprintf fmt "@{<magenta>apply@} %a" (Tuple.print print_expr)
        (Tuple.make [] [ ("f", f); ("arg", arg) ])
  | E_Assign { assignee; value } ->
      fprintf fmt
        "@{<magenta>assign@} (@;<0 2>@[<v>assignee = %a,@]@;<0 2>@[<v>value = %a@]@ )"
        print_assignee_expr assignee print_expr value

and print_expr : formatter -> expr -> unit =
 fun fmt expr -> print_expr_shape fmt expr.shape

(* ASSIGNEE EXPR *)

and print_assignee_expr_shape : formatter -> assignee_expr_shape -> unit =
 fun fmt -> function
  | A_Placeholder -> fprintf fmt "@{<magenta>_@}"
  | A_Binding binding ->
      fprintf fmt "@{<magenta>binding@} %a" print_binding binding
  | A_Let pattern -> fprintf fmt "@{<magenta>let@} %a" print_pattern pattern

and print_assignee_expr : formatter -> assignee_expr -> unit =
 fun fmt expr -> print_assignee_expr_shape fmt expr.shape

(* PATTERN *)

and print_pattern_shape : formatter -> pattern_shape -> unit =
 fun fmt -> function
  | P_Placeholder -> fprintf fmt "@{<magenta>_@}"
  | P_Binding binding ->
      fprintf fmt "@{<magenta>binding@} %a" print_binding binding

and print_pattern : formatter -> pattern -> unit =
 fun fmt pattern -> print_pattern_shape fmt pattern.shape

(* OTHER *)
and print_binding : formatter -> binding -> unit =
 fun fmt binding -> fprintf fmt "%a" String.print_maybe_escaped binding.name
