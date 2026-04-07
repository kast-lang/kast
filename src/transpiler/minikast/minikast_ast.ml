open Std
open Kast_util

type expr =
  | Unit
  | Uninitialized
  | Let of
      { var : string
      ; value : expr
      }
  | Obj of field list
  | Variant of string
  | Claim of place_expr
  | Cast of
      { value : expr
      ; target : ty
      }
  | Ref of place_expr
  | Native of native_expr
  | Fn of fn_def
  | And of expr * expr
  | Or of expr * expr
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Float64 of float
  | Char of Uchar.t
  | String of string
  | Loop of expr
  | InjectContext of
      { name : string
      ; value : expr
      }
  | EnumIs of
      { value : expr
      ; expected : string
      }
  | Unwindable of
      { token_ident : string
      ; body : expr
      }
  | Unwind of
      { token : expr
      ; value : expr
      }
  | Stmt of expr
  | Then of expr list
  | Assign of
      { assignee : place_expr
      ; value : expr
      }
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
  | TypeAscribed of
      { expr : expr
      ; ty : ty
      }

and field =
  { name : string
  ; value : expr
  }

and place_expr =
  | Ident of string
  | Field of
      { obj : place_expr
      ; field : string
      }
  | CurrentContext of string
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
  | Alias of ty

and ty =
  | Named of string
  | Unit
  | Int32
  | Int64
  | Float64
  | Bool
  | String
  | List of ty
  | Char
  | Ref of ty
  | Fn of fn_ty
  | UnwindToken of ty
  | Any

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

and const =
  { name : string
  ; ty : ty
  ; value : expr
  }

and program =
  { types : ty_def StringMap.t
  ; fns : fn_def StringMap.t
  ; contexts : ty StringMap.t
  ; consts : const list
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

  type priority =
    | Any
    | Stmt
    | Assign
    | Ascribe
    | Fn
    | If
    | Unwind
    | Cast
    | Or
    | And
    | Compare
    | Ref
    | Field

  let rec need_surround_place_expr (pp : priority) (place : place_expr) : bool =
    match place with
    | Ident _ -> false
    | Field _ -> pp > Field
    | CurrentContext _ -> false
    | Deref _ -> pp > Field
    | Temp expr -> need_surround_expr pp expr

  and need_surround_expr (pp : priority) (expr : expr) : bool =
    match expr with
    | Unit -> false
    | Uninitialized -> false
    | Let _ -> pp >= Assign
    | Obj _ -> false
    | Variant _ -> false
    | Claim place -> need_surround_place_expr pp place
    | Cast _ -> pp > Cast
    | Ref _ -> pp > Ref
    | Native _ -> false
    | Fn _ -> pp > Fn
    | And (_, _) -> pp > And
    | Or (_, _) -> pp > Or
    | Bool _ -> false
    | Int32 _ -> false
    | Int64 _ -> false
    | Float64 _ -> false
    | Char _ -> false
    | String _ -> false
    | Loop _ -> false
    | InjectContext _ -> pp > Stmt
    | EnumIs _ -> pp >= Compare
    | Unwindable _ -> false
    | Unwind _ -> pp >= Unwind
    | Stmt _ -> pp > Stmt
    | Then _ -> pp > Stmt
    | Assign _ -> pp >= Assign
    | Scope _ -> false
    | If _ -> pp >= If
    | Apply _ -> pp > Field
    | TypeAscribed _ -> pp >= Ascribe
  ;;

  let rec _unused = ()

  and print_ty (ty : ty) =
    match ty with
    | Named name -> write name
    | Unit -> write "()"
    | Int32 -> write "Int32"
    | Int64 -> write "Int64"
    | Float64 -> write "Float64"
    | Bool -> write "Bool"
    | String -> write "String"
    | Char -> write "Char"
    | Ref referenced ->
      write "&";
      print_ty referenced
    | List element_ty ->
      write "List[";
      print_ty element_ty;
      write "]"
    | UnwindToken ty ->
      write "UnwindToken[";
      print_ty ty;
      write "]"
    | Fn { args; result } ->
      write "(";
      args
      |> List.iteri (fun i arg ->
        if i <> 0 then write ", ";
        print_ty arg);
      write ") -> ";
      print_ty result
    | Any -> write "Any"

  and maybe_surround surround f =
    if surround
    then (
      write "(";
      writeln ();
      inc_indentation ());
    f ();
    if surround
    then (
      dec_indentation ();
      writeln ();
      write ")")

  and print_place_expr (pp : priority) (expr : place_expr) =
    let surround = need_surround_place_expr pp expr in
    maybe_surround surround (fun () ->
      match expr with
      | Ident name -> write name
      | Field { obj; field } ->
        print_place_expr Field obj;
        write ".";
        write field
      | CurrentContext name ->
        write "@current ";
        write name
      | Deref expr ->
        print_expr Field expr;
        write "^"
      | Temp expr -> print_expr pp expr)

  and print_expr (pp : priority) (expr : expr) =
    let surround = need_surround_expr pp expr in
    maybe_surround surround (fun () ->
      match expr with
      | Unit -> write "()"
      | Bool x -> write (Bool.to_string x)
      | Int32 x -> write (Int32.to_string x)
      | Int64 x -> write (Int64.to_string x)
      | Float64 x -> write (Float.to_string x)
      | Char x ->
        let s = make_string "%a" Uchar.print_debug x in
        write s
      | String s ->
        let s = make_string "%a" String.print_debug s in
        write s
      | Uninitialized -> write "uninitialized"
      | And (a, b) ->
        print_expr And a;
        write " and ";
        print_expr And b
      | Or (a, b) ->
        print_expr Or a;
        write " or ";
        print_expr Or b
      | Claim place -> print_place_expr pp place
      | EnumIs { value; expected } ->
        print_expr Compare value;
        write " == :";
        write expected
      | InjectContext { name; value } ->
        write "with ";
        write name;
        write " = ";
        print_expr Assign value
      | Variant name ->
        write ":";
        write name
      | Loop body ->
        write "@loop (";
        inc_indentation ();
        print_expr Any body;
        dec_indentation ();
        write ")"
      | TypeAscribed { expr; ty } ->
        print_expr Ascribe expr;
        write " :: ";
        print_ty ty
      | Assign { assignee; value } ->
        print_place_expr Assign assignee;
        write " = ";
        print_expr Assign value
      | Unwindable { token_ident; body } ->
        write "unwindable ";
        write token_ident;
        write " (";
        writeln ();
        inc_indentation ();
        print_expr Any body;
        dec_indentation ();
        write ")"
      | Unwind { token; value } ->
        write "unwind ";
        print_expr Unwind token;
        write " with ";
        print_expr Unwind value
      | Let { var; value } ->
        write "let ";
        write var;
        write " = ";
        print_expr Assign value
      | Ref place ->
        write "&";
        print_place_expr Ref place
      | Obj fields ->
        write "{";
        writeln ();
        inc_indentation ();
        fields
        |> List.iter (fun field ->
          write ".";
          write field.name;
          write " = ";
          print_expr Assign field.value;
          write ",";
          writeln ());
        dec_indentation ();
        write "}"
      | Native { parts } ->
        write "@native \"";
        parts
        |> List.iter (function
          | Raw s ->
            write (make_string "%a" (String.print_escaped_content ~in_string:true) s)
          | Interpolated expr ->
            write "\\(";
            print_expr Any expr;
            write ")");
        write "\""
      | Fn def -> print_fn def
      | Stmt expr ->
        print_expr Stmt expr;
        write ";"
      | Then exprs ->
        exprs
        |> List.iteri (fun i expr ->
          if i <> 0
          then (
            write ";";
            writeln ());
          print_expr Stmt expr)
      | Cast { value; target } ->
        print_expr Cast value;
        write " as ";
        print_ty target
      | Scope expr ->
        write "(";
        writeln ();
        inc_indentation ();
        print_expr Any expr;
        writeln ();
        dec_indentation ();
        write ")"
      | If { cond; then_case; else_case } ->
        write "if ";
        print_expr If cond;
        write " then ";
        print_expr If then_case;
        (match else_case with
         | None -> ()
         | Some else_case ->
           write " else ";
           print_expr If else_case)
      | Apply { f; args } ->
        print_expr Field f;
        write "(";
        args
        |> List.iteri (fun i arg ->
          if i <> 0 then write ", ";
          print_expr Any arg);
        write ")")

  and print_fn (def : fn_def) =
    write "(";
    def.args
    |> List.iteri (fun i (arg : fn_arg) ->
      if i <> 0 then write ", ";
      write arg.name;
      write " :: ";
      print_ty arg.ty);
    write ") -> ";
    print_ty def.result_ty;
    write " => ";
    print_expr Fn def.body

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
         write "}"
       | Alias ty ->
         write "type ";
         print_ty ty);
      write ";";
      writeln ());
    program.contexts
    |> StringMap.iter (fun name ty ->
      write "const ";
      write name;
      write " = @context ";
      print_ty ty;
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
    |> List.iter (fun (const : const) ->
      write "const ";
      write const.name;
      write " :: ";
      print_ty const.ty;
      write " = ";
      print_expr Assign const.value;
      write ";";
      writeln ())
  ;;
end
