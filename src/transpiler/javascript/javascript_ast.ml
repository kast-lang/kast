open Std
open Kast_util

type name = { raw : string }

let gen_name prefix : name =
  let id = Id.gen () in
  { raw = make_string "%s_%d" prefix id.value }

type expr =
  | Undefined
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Var of name
  | Fn of {
      args : name list;
      body : stmt list;
    }
  | Call of {
      f : expr;
      args : expr list;
    }
  | Raw of string
  | Obj of obj_part list
  | Field of {
      obj : expr;
      field : string;
    }
  | Not of expr
  | Compare of {
      op : compare_op;
      lhs : expr;
      rhs : expr;
    }

and compare_op =
  | Less
  | LessOrEqual
  | Equal
  | NotEqual
  | GreaterOrEqual
  | Greater

and obj_part =
  | Field of {
      name : string;
      value : expr;
    }
  | Unpack of expr

and stmt =
  | Expr of expr
  | Let of {
      var : name;
      value : expr;
    }
  | Assign of {
      assignee : expr;
      value : expr;
    }
  | Return of expr
  | Raise of expr
  | If of {
      condition : expr;
      then_case : stmt list;
      else_case : stmt list option;
    }

module Precedence = struct
  type t =
    | None
    | CalledFn
    | Assignee
    | Assigned
    | FnArg
    | Stmt
    | Raised
    | IfCondition
    | Field
    | Unpack
    | Obj
    | Returned
    | CmpArg
    | Not
end

let print_cmp_op fmt op =
  fprintf fmt "%s"
    (match op with
    | Less -> "<"
    | LessOrEqual -> "<="
    | Equal -> "==="
    | NotEqual -> "!=="
    | GreaterOrEqual -> ">="
    | Greater -> ">")

let rec print_expr ~(precedence : Precedence.t) fmt expr =
  let surround_with_parens = true in
  if surround_with_parens then fprintf fmt "(";
  (match expr with
  | Raw s -> fprintf fmt "@{<cyan>%s@}" s
  | Undefined -> fprintf fmt "@{<magenta>undefined@}"
  | Null -> fprintf fmt "@{<magenta>null@}"
  | Bool b -> fprintf fmt "@{<magenta>%b@}" b
  | Number x -> fprintf fmt "@{<italic>%g@}" x
  | String s -> fprintf fmt "@{<green>%S@}" s
  | Var name -> print_name fmt name
  | Fn { args; body } ->
      fprintf fmt "(";
      args
      |> List.iteri (fun i name ->
          if i <> 0 then fprintf fmt ",";
          print_name fmt name);
      fprintf fmt ") => {%a}" print_stmts body
  | Call { f; args } ->
      fprintf fmt "%a(" (print_expr ~precedence:CalledFn) f;
      args
      |> List.iteri (fun i arg ->
          if i <> 0 then fprintf fmt ",";
          print_expr ~precedence:FnArg fmt arg);
      fprintf fmt ")"
  | Obj fields ->
      fprintf fmt "{";
      fields
      |> List.iteri (fun i part ->
          if i <> 0 then fprintf fmt ",";
          print_obj_part fmt part);
      fprintf fmt "}"
  | Field { obj; field } ->
      fprintf fmt "%a[%S]" (print_expr ~precedence:Obj) obj field
  | Not expr -> fprintf fmt "!%a" (print_expr ~precedence:Not) expr
  | Compare { op; lhs; rhs } ->
      fprintf fmt "%a %a %a"
        (print_expr ~precedence:CmpArg)
        lhs print_cmp_op op
        (print_expr ~precedence:CmpArg)
        rhs);
  if surround_with_parens then fprintf fmt ")"

and print_obj_part fmt = function
  | Field { name; value } ->
      fprintf fmt "%S: %a" name (print_expr ~precedence:Field) value
  | Unpack packed -> fprintf fmt "...%a" (print_expr ~precedence:Unpack) packed

and print_stmts fmt stmts =
  stmts |> List.iter (fun stmt -> fprintf fmt "%a;" print_stmt stmt)

and print_stmt fmt = function
  | Expr e -> print_expr ~precedence:Stmt fmt e
  | Return e ->
      fprintf fmt "@{<magenta>return@} %a" (print_expr ~precedence:Returned) e
  | Let { var; value } ->
      fprintf fmt "@{<magenta>let@} %a = %a" print_name var
        (print_expr ~precedence:Assigned)
        value
  | Assign { assignee; value } ->
      fprintf fmt "%a = %a"
        (print_expr ~precedence:Assignee)
        assignee
        (print_expr ~precedence:Assigned)
        value
  | Raise expr ->
      fprintf fmt "@{<magenta>raise@} %a" (print_expr ~precedence:Raised) expr
  | If { condition; then_case; else_case } -> (
      fprintf fmt "@{<magenta>if@} %a {%a}"
        (print_expr ~precedence:IfCondition)
        condition print_stmts then_case;
      match else_case with
      | None -> ()
      | Some else_case ->
          fprintf fmt " @{<magenta>else@} {%a}" print_stmts else_case)

and print_name fmt name = fprintf fmt "%s" name.raw
