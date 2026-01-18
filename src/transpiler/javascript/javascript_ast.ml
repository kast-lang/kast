open Std
open Kast_util

(* let a = 2 *)

type original_name =
  { raw : string
  ; span : span
  }

type name =
  { raw : string
  ; original : original_name option
  }

let gen_name ~original prefix : name =
  let id = Id.gen () in
  { raw = make_string "%s_%d" prefix id.value; original }
;;

type expr_shape =
  | Undefined
  | Null
  | Bool of bool
  | Number of float
  | Bigint of string
  | String of string
  | List of expr list
  | Var of name
  | Fn of
      { async : bool
      ; args : name list
      ; body : stmt list
      }
  | Call of
      { async : bool
      ; f : expr
      ; args : expr list
      }
  | Raw of string
  | Obj of obj_part list
  | Field of
      { obj : expr
      ; field : string
      }
  | Not of expr
  | Compare of
      { op : compare_op
      ; lhs : expr
      ; rhs : expr
      }
  | BinOp of
      { op : bin_op
      ; lhs : expr
      ; rhs : expr
      }

and expr =
  { shape : expr_shape
  ; span : span option
  }

and bin_op =
  | And
  | Or

and compare_op =
  | Less
  | LessOrEqual
  | Equal
  | NotEqual
  | GreaterOrEqual
  | Greater

and obj_part =
  | Field of
      { name : string
      ; value : expr
      }
  | Unpack of expr

and stmt_shape =
  | Block of stmt list
  | Labelled of
      { label : name
      ; stmt : stmt
      }
  | Expr of expr
  | Let of
      { var : name
      ; value : expr
      }
  | Assign of
      { assignee : expr
      ; value : expr
      }
  | LabelledBreak of name
  | Return of expr
  | Throw of expr
  | Try of
      { body : stmt list
      ; catch_var : name
      ; catch_body : stmt list
      }
  | If of
      { condition : expr
      ; then_case : stmt list
      ; else_case : stmt list option
      }
  | For of
      { init : expr option
      ; cond : expr option
      ; after : expr option
      ; body : stmt list
      }
  | Raw of string

and stmt =
  { shape : stmt_shape
  ; span : span option
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
    | ForArg
    | Field
    | Unpack
    | Obj
    | Returned
    | CmpArg
    | BinOpArg
    | Not
end

let write = Writer.write_string
let write_newline = Writer.write_newline
let write_spanned = Writer.write_spanned

let write_maybe_spanned span f writer =
  match span with
  | None -> f ()
  | Some span -> write_spanned span f writer
;;

let print_cmp_op writer op =
  writer
  |> write
       (match op with
        | Less -> "<"
        | LessOrEqual -> "<="
        | Equal -> "==="
        | NotEqual -> "!=="
        | GreaterOrEqual -> ">="
        | Greater -> ">")
;;

let print_bin_op writer op =
  writer
  |> write
       (match op with
        | And -> "&&"
        | Or -> "||")
;;

let rec print_expr ~(precedence : Precedence.t) (writer : Writer.t) (expr : expr) =
  writer
  |> write_maybe_spanned expr.span (fun () ->
    let surround_with_parens = true in
    if surround_with_parens then writer |> write "(";
    (match expr.shape with
     | Raw s -> writer |> write s
     | Undefined -> writer |> write "undefined"
     | Null -> writer |> write "null"
     | Bool b ->
       writer
       |> write
            (match b with
             | true -> "true"
             | false -> "false")
     | Number x -> writer |> write (make_string "%.20g" x)
     | Bigint x ->
       writer |> write x;
       writer |> write "n"
     | String s -> writer |> write (make_string "%S" s)
     | List a ->
       writer |> write "[";
       a
       |> List.iteri (fun i x ->
         if i <> 0 then writer |> write ",";
         print_expr ~precedence:FnArg writer x);
       writer |> write "]"
     | Var name -> print_name writer name
     | Fn { async; args; body } ->
       if async then writer |> write "async ";
       writer |> write "(";
       args
       |> List.iteri (fun i name ->
         if i <> 0 then writer |> write ",";
         print_name writer name);
       writer |> write ") => ";
       print_block writer body
     | Call { async; f; args } ->
       if async then writer |> write "await ";
       print_expr ~precedence:CalledFn writer f;
       writer |> write "(";
       args
       |> List.iteri (fun i arg ->
         if i <> 0 then writer |> write ",";
         print_expr ~precedence:FnArg writer arg);
       writer |> write ")"
     | Obj fields ->
       writer |> write "{";
       fields
       |> List.iteri (fun i part ->
         if i <> 0 then writer |> write ",";
         print_obj_part writer part);
       writer |> write "}"
     | Field { obj; field } ->
       print_expr ~precedence:Obj writer obj;
       writer |> write "[";
       writer |> write (make_string "%S" field);
       writer |> write "]"
     | Not expr ->
       writer |> write "!";
       print_expr ~precedence:Not writer expr
     | Compare { op; lhs; rhs } ->
       print_expr ~precedence:CmpArg writer lhs;
       print_cmp_op writer op;
       print_expr ~precedence:CmpArg writer rhs
     | BinOp { op; lhs; rhs } ->
       print_expr ~precedence:BinOpArg writer lhs;
       print_bin_op writer op;
       print_expr ~precedence:BinOpArg writer rhs);
    if surround_with_parens then writer |> write ")")

and print_obj_part writer = function
  | Field { name; value } ->
    writer |> write (make_string "%S" name);
    writer |> write ":";
    print_expr ~precedence:Field writer value
  | Unpack packed ->
    writer |> write "...";
    print_expr ~precedence:Unpack writer packed

and print_block writer (stmts : stmt list) =
  let old_prefix = writer.line_prefix in
  writer.line_prefix <- old_prefix ^ "  ";
  writer |> write "{";
  writer |> write_newline;
  stmts
  |> List.iteri (fun i stmt ->
    if i <> 0 then writer |> write_newline;
    print_stmt writer stmt;
    writer |> write ";");
  writer.line_prefix <- old_prefix;
  writer |> write_newline;
  writer |> write "}"

and print_toplevel_stmts writer (stmts : stmt list) =
  stmts
  |> List.iter (fun stmt ->
    print_stmt writer stmt;
    writer |> write ";";
    writer |> write_newline)

and print_stmt writer (stmt : stmt) =
  writer
  |> write_maybe_spanned stmt.span (fun () ->
    match stmt.shape with
    | Labelled { label; stmt } ->
      print_name writer label;
      writer |> write ":";
      print_stmt writer stmt
    | Block stmts -> print_block writer stmts
    | LabelledBreak label ->
      writer |> write "break ";
      print_name writer label
    | Expr e -> print_expr ~precedence:Stmt writer e
    | Return e ->
      writer |> write "return ";
      print_expr ~precedence:Returned writer e
    | Let { var; value } ->
      writer |> write "let ";
      print_name writer var;
      writer |> write " = ";
      print_expr ~precedence:Assigned writer value
    | Assign { assignee; value } ->
      print_expr ~precedence:Assignee writer assignee;
      writer |> write " = ";
      print_expr ~precedence:Assigned writer value
    | Try { body; catch_var; catch_body } ->
      writer |> write "try ";
      print_block writer body;
      writer |> write " catch (";
      print_name writer catch_var;
      writer |> write ") ";
      print_block writer catch_body
    | Throw expr ->
      writer |> write "throw ";
      print_expr ~precedence:Raised writer expr
    | If { condition; then_case; else_case } ->
      writer |> write "if (";
      print_expr ~precedence:IfCondition writer condition;
      writer |> write ") ";
      print_block writer then_case;
      (match else_case with
       | None -> ()
       | Some else_case ->
         writer |> write " else ";
         print_block writer else_case)
    | For { init; cond; after; body } ->
      writer |> write "for (";
      maybe_print (print_expr ~precedence:ForArg) writer init;
      writer |> write ";";
      maybe_print (print_expr ~precedence:ForArg) writer cond;
      writer |> write ";";
      maybe_print (print_expr ~precedence:ForArg) writer after;
      writer |> write ") ";
      print_block writer body
    | Raw s -> writer |> write s)

and maybe_print : 'a. (Writer.t -> 'a -> unit) -> Writer.t -> 'a option -> unit =
  fun f writer opt ->
  match opt with
  | None -> ()
  | Some x -> f writer x

and print_name writer name =
  match name.original with
  | Some original ->
    writer |> Writer.write_name ~original_name:original.raw ~span:original.span name.raw
  | None -> writer |> Writer.write_string name.raw
;;
