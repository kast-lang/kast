open Std
open Kast_util

type expr =
  | Unit
  | Uninitialized
  | Let of
      { var : string
      ; value : expr
      }
  | Claim of place_expr
  | Ref of place_expr
  | Native of native_expr
  | Fn of fn_def
  | Bool of bool
  | String of string
  | Stmt of expr
  | Then of expr list
  | Scope of expr
  | If of
      { cond : expr
      ; then_case : expr
      ; else_case : expr option
      }
  | Apply of
      { f : expr
      ; args : expr list
      }

and place_expr =
  | Ident of string
  | Field of
      { obj : place_expr
      ; field : string
      }
  | Deref of expr
  | Temp of expr

and native_expr = { parts : native_expr_part list }

and native_expr_part =
  | Raw of string
  | Interpolated of expr

and ty_def =
  | Enum of StringSet.t
  | Struct of ty StringMap.t
  | Union of ty StringMap.t

and ty =
  | Named of string
  | Unit
  | Int32
  | Bool
  | String
  | Char
  | Ref of ty
  | Fn of fn_ty

and fn_ty =
  { args : ty list
  ; result : ty
  }

and fn_def =
  { args : fn_arg list
  ; result_ty : ty
  ; body : expr
  }

and fn_arg =
  { name : string
  ; ty : ty
  }

and program =
  { types : ty_def StringMap.t
  ; fns : fn_def StringMap.t
  ; consts : expr StringMap.t
  }

module Print = struct
  let indentation = ref 0
  let written_after_newline = ref false
  let inc_indentation () = indentation := !indentation + 1
  let dec_indentation () = indentation := !indentation - 1

  let write s =
    if not !written_after_newline
    then (
      written_after_newline := true;
      let i = ref !indentation in
      while !i > 0 do
        print_string "    ";
        i := !i - 1
      done);
    print_string s
  ;;

  let writeln () =
    print_newline ();
    written_after_newline := false
  ;;

  let rec _unused = ()

  and print_ty (ty : ty) =
    match ty with
    | Named name -> write name
    | Unit -> write "()"
    | Int32 -> write "Int32"
    | Bool -> write "Bool"
    | String -> write "String"
    | Char -> write "Char"
    | Ref referenced ->
      write "&";
      print_ty referenced
    | Fn { args; result } ->
      write "(";
      args
      |> List.iteri (fun i arg ->
        if i <> 0 then write ", ";
        print_ty arg);
      write ") -> ";
      print_ty result

  and print_place_expr (expr : place_expr) =
    match expr with
    | Ident name -> write name
    | Field { obj; field } ->
      print_place_expr obj;
      write ".";
      write field
    | Deref expr ->
      print_expr expr;
      write "^"
    | Temp expr -> print_expr expr

  and print_expr (expr : expr) =
    match expr with
    | Unit -> write "()"
    | Bool x -> write (make_string "%b" x)
    | Uninitialized -> write "uninitialized"
    | Claim place -> print_place_expr place
    | Let { var; value } ->
      write "let ";
      write var;
      write " = ";
      print_expr value
    | Ref place ->
      write "&";
      print_place_expr place
    | Native { parts } ->
      write "@native \"";
      parts
      |> List.iter (function
        | Raw s ->
          write (make_string "%a" (String.print_escaped_content ~in_string:true) s)
        | Interpolated expr ->
          write "\\(";
          print_expr expr;
          write ")");
      write "\""
    | Fn def -> print_fn def
    | String s ->
      let s = make_string "%a" String.print_debug s in
      write s
    | Stmt expr ->
      print_expr expr;
      write ";"
    | Then exprs ->
      exprs
      |> List.iteri (fun i expr ->
        if i <> 0
        then (
          write ";";
          writeln ());
        print_expr expr)
    | Scope expr ->
      write "(";
      writeln ();
      inc_indentation ();
      print_expr expr;
      writeln ();
      dec_indentation ();
      write ")"
    | If { cond; then_case; else_case } ->
      write "if ";
      print_expr cond;
      write " then ";
      print_expr then_case;
      (match else_case with
       | None -> ()
       | Some else_case ->
         write " else ";
         print_expr else_case)
    | Apply { f; args } ->
      print_expr f;
      write "(";
      args
      |> List.iteri (fun i arg ->
        if i <> 0 then write ", ";
        print_expr arg);
      write ")"

  and print_fn (def : fn_def) =
    write "(";
    def.args
    |> List.iteri (fun i arg ->
      if i <> 0 then write ", ";
      write arg.name;
      write " :: ";
      print_ty arg.ty);
    write ") -> ";
    print_ty def.result_ty;
    write " => ";
    print_expr def.body

  and print_program (program : program) =
    program.types
    |> StringMap.iter (fun name def ->
      write "const ";
      write name;
      write " = ";
      (match def with
       | Enum variants ->
         write "enum {";
         writeln ();
         inc_indentation ();
         variants
         |> StringSet.iter (fun variant ->
           write "| :";
           write variant;
           writeln ());
         dec_indentation ();
         write "}"
       | Struct fields ->
         write "struct {";
         writeln ();
         inc_indentation ();
         fields
         |> StringMap.iter (fun name ty ->
           write ".";
           write name;
           write " :: ";
           print_ty ty;
           write ",";
           writeln ());
         dec_indentation ();
         write "}"
       | Union variants ->
         write "union {";
         writeln ();
         inc_indentation ();
         variants
         |> StringMap.iter (fun name ty ->
           write ".";
           write name;
           write " :: ";
           print_ty ty;
           write ",";
           writeln ());
         dec_indentation ();
         write "}");
      write ";";
      writeln ());
    program.fns
    |> StringMap.iter (fun name def ->
      write "const ";
      write name;
      write " = ";
      print_fn def;
      write ";";
      writeln ());
    program.consts
    |> StringMap.iter (fun name def ->
      write "const ";
      write name;
      write " = ";
      print_expr def;
      write ";";
      writeln ())
  ;;
end
