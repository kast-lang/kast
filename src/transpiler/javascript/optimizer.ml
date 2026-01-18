open Std
open Kast_util
open Javascript_ast

let rec optimize_expr : expr -> expr =
  fun ({ shape; span } as original) ->
  match shape with
  | Undefined -> original
  | Null -> original
  | Bool _ -> original
  | Number _ -> original
  | Bigint _ -> original
  | String _ -> original
  | List list -> { shape = List (list |> List.map optimize_expr); span }
  | Var _ -> original
  | Fn { async; args; body } ->
    { shape = Fn { async; args; body = optimize_stmts body }; span }
  | Call { async; f; args } ->
    let f = optimize_expr f in
    let args = args |> List.map optimize_expr in
    { shape = Call { async; f; args }; span }
  | Raw _ -> original
  | Obj parts -> { shape = Obj (parts |> List.map optimize_obj_part); span }
  | Field { obj; field } -> { shape = Field { obj = optimize_expr obj; field }; span }
  | Not expr -> { shape = Not (optimize_expr expr); span }
  | Compare { op; lhs; rhs } ->
    { shape = Compare { op; lhs = optimize_expr lhs; rhs = optimize_expr rhs }; span }
  | BinOp { op; lhs; rhs } ->
    { shape = BinOp { op; lhs = optimize_expr lhs; rhs = optimize_expr rhs }; span }

and optimize_expr_as_stmts : span:span option -> expr -> stmt list =
  fun ~span expr ->
  let expr = optimize_expr expr in
  match expr with
  | { shape =
        Call
          { async = call_async
          ; f = { shape = Fn { async = f_async; args = f_args; body }; span = _ }
          ; args = call_args
          }
    ; span = _
    }
    when false && Bool.equal call_async f_async ->
    let result =
      (List.zip f_args call_args
       |> List.map (fun (f_arg, call_arg) ->
         { shape = Let { var = f_arg; value = call_arg }; span = None }))
      @ body
    in
    result
  | _ -> [ { shape = Expr (optimize_expr expr); span } ]

and optimize_obj_part : obj_part -> obj_part =
  fun part ->
  match part with
  | Field { name; value } -> Field { name; value = optimize_expr value }
  | Unpack expr -> Unpack (optimize_expr expr)

and optimize_stmts : stmt list -> stmt list =
  fun stmts -> stmts |> List.map optimize_stmt |> List.flatten

and optimize_stmt : stmt -> stmt list =
  fun ({ shape; span } as original) ->
  match shape with
  | Labelled { label; stmt } ->
    let stmt =
      match optimize_stmt stmt with
      | [ stmt ] -> stmt
      | stmts -> { shape = Block stmts; span }
    in
    [ { shape = Labelled { label; stmt }; span } ]
  | Block stmts -> [ { shape = Block (optimize_stmts stmts); span } ]
  | LabelledBreak _label -> [ original ]
  | Expr expr -> optimize_expr_as_stmts ~span expr
  | Let { var; value } -> [ { shape = Let { var; value = optimize_expr value }; span } ]
  | Assign { assignee; value } ->
    [ { shape = Assign { assignee; value = optimize_expr value }; span } ]
  | Return value -> [ { shape = Return (optimize_expr value); span } ]
  | Throw value -> [ { shape = Throw (optimize_expr value); span } ]
  | Try { body; catch_var; catch_body } ->
    [ { shape =
          Try
            { body = optimize_stmts body
            ; catch_var
            ; catch_body = optimize_stmts catch_body
            }
      ; span
      }
    ]
  | If { condition; then_case; else_case } ->
    [ { shape =
          If
            { condition = optimize_expr condition
            ; then_case = optimize_stmts then_case
            ; else_case = else_case |> Option.map optimize_stmts
            }
      ; span
      }
    ]
  | For { init; cond; after; body } ->
    [ { shape =
          For
            { init = init |> Option.map optimize_expr
            ; cond = cond |> Option.map optimize_expr
            ; after = after |> Option.map optimize_expr
            ; body = body |> optimize_stmts
            }
      ; span
      }
    ]
  | Raw _ -> [ original ]
;;
