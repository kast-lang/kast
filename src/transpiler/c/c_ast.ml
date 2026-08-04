open Std
open Kast_util

type literal =
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Float64 of float
  | Char of Uchar.t
  | String of string

and expr =
  | Unit
  | Literal of literal
  | Native of native_expr
  | Claim of place_expr
  | Cast of
      { value : expr
      ; target : ty
      }
  | AddrOf of place_expr
  | And of expr * expr
  | Or of expr * expr
  | Equal of expr * expr
  | Apply of
      { f : expr
      ; args : expr list
      }
  | Block of block

and stmt =
  | Native of native_expr
  | Comment of string
  | DeclareVar of
      { name : string
      ; ty : ty
      }
  | Expr of expr
  | If of
      { cond : expr
      ; then_case : block
      ; else_case : block option
      }
  | Assign of
      { assignee : place_expr
      ; value : expr
      }
  | Goto of { label : string }
  | GotoLabel of string
  | For of { body : block }
  | Return of expr

and field =
  { name : string
  ; value : expr
  }

and place_expr =
  | Ident of string
  | Native of native_expr
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

and ty_def_shape =
  | Enum of StringSet.t
  | Struct of ty StringMap.t
  | Union of ty StringMap.t
  | Fn of
      { args : ty list
      ; result_ty : ty
      }
  | Alias of ty

and ty_def =
  { shape : ty_def_shape
  ; comment : string option
  }

and ty =
  | Unit
  | Raw of string
  | Named of string
  | Ptr of ty
  | Void

and block = stmt list

and fn_ty =
  { args : ty list
  ; result : ty
  }

and fn_def =
  { comment : string option
  ; args : fn_arg list
  ; result_ty : ty
  ; body : block
  }

and fn_arg =
  { name : string
  ; ty : ty
  }

and static =
  { name : string
  ; ty : ty
  ; comment : string option
  }

and program =
  { includes : StringSet.t
  ; types : ty_def StringMap.t
  ; fns : fn_def StringMap.t
  ; statics : static list
  }

and declared_state =
  | BeingDeclared
  | Completed

module Print = struct
  let indentation = ref 0
  let written_after_newline = ref false
  let inc_indentation () = indentation := !indentation + 1
  let dec_indentation () = indentation := !indentation - 1

  type _ Effect.t += GetOutput : out_channel Effect.t

  let print_string s =
    let out = Effect.perform GetOutput in
    output_string out s
  ;;

  let print_newline () = print_string "\n"

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

  let rec need_surround_place_expr (place : place_expr) : bool =
    match place with
    | Ident _ -> false
    | Temp expr -> need_surround_expr expr
    | _ -> true

  and need_surround_expr (expr : expr) : bool =
    match expr with
    | Claim expr -> need_surround_place_expr expr
    | Literal _ -> false
    | _ -> true
  ;;

  let write_comment (comment : string option) =
    match comment with
    | Some comment ->
      write "/*";
      writeln ();
      write comment;
      writeln ();
      write "*/";
      writeln ()
    | None -> ()
  ;;

  let rec _unused = ()

  and print_ty (ty : ty) =
    match ty with
    | Unit -> write "_Unit"
    | Raw s -> write s
    | Named name -> write name
    | Ptr referenced ->
      print_ty referenced;
      write "*"
    | Void -> write "void"

  and maybe_surround surround f =
    if surround then write "(";
    f ();
    if surround then write ")"

  and print_place_expr (expr : place_expr) : unit =
    let surround = need_surround_place_expr expr in
    maybe_surround surround (fun () ->
      match expr with
      | Ident name -> write name
      | Native native -> print_native native
      | Field { obj; field } ->
        print_place_expr obj;
        write ".";
        write field
      | Deref expr ->
        write "*";
        print_expr expr
      | Temp expr -> print_expr expr)

  and print_stmt (stmt : stmt) : unit =
    match stmt with
    | Native native -> print_native native
    | Comment s ->
      write "/* ";
      write s;
      write " */"
    | DeclareVar { name; ty } ->
      print_ty ty;
      write " ";
      write name
    | Expr expr -> print_expr expr
    | If { cond; then_case; else_case } ->
      write "if (";
      print_expr cond;
      write ") ";
      print_block then_case;
      (match else_case with
       | Some else_case ->
         write " else ";
         print_block else_case
       | None -> ())
    | Assign { assignee; value } ->
      print_place_expr assignee;
      write " = ";
      print_expr value
    | Goto { label } ->
      write "goto ";
      write label
    | GotoLabel label ->
      write label;
      write ":"
    | For { body } ->
      write "for(;;) ";
      print_block body
    | Return value ->
      write "return ";
      print_expr value

  and print_native ({ parts } : native_expr) : unit =
    parts
    |> List.iter (function
      | Raw s -> write s
      | Interpolated expr -> print_expr expr)

  and print_expr (expr : expr) : unit =
    let surround = need_surround_expr expr in
    maybe_surround surround (fun () ->
      match expr with
      | Unit -> write "(_Unit){}"
      | Literal lit ->
        write
          (match lit with
           | Bool x -> Bool.to_string x
           | Int32 x -> Int32.to_string x
           | Int64 x -> Int64.to_string x
           | Float64 x -> Float.to_string x
           | Char x -> make_string "%a" Uchar.print_debug x
           | String s -> make_string "%a" String.print_debug s)
      | And (a, b) ->
        print_expr a;
        write " && ";
        print_expr b
      | Or (a, b) ->
        print_expr a;
        write " || ";
        print_expr b
      | Equal (a, b) ->
        print_expr a;
        write " == ";
        print_expr b
      | Claim place -> print_place_expr place
      | AddrOf place ->
        write "&";
        print_place_expr place
      | Native native -> print_native native
      | Cast { value; target } ->
        write "(";
        print_ty target;
        write ")";
        print_expr value
      | Apply { f; args } ->
        print_expr f;
        write "(";
        args
        |> List.iteri (fun i arg ->
          if i <> 0 then write ", ";
          print_expr arg);
        write ")"
      | Block block -> print_block block)

  and print_fn_sig ~(end_with_semicolon : bool) (name : string) (def : fn_def) =
    write_comment def.comment;
    print_ty def.result_ty;
    write " ";
    write name;
    write "(";
    def.args
    |> List.iteri (fun i (arg : fn_arg) ->
      if i <> 0 then write ", ";
      print_ty arg.ty;
      write " ";
      write arg.name);
    write ")";
    if end_with_semicolon
    then (
      write ";";
      writeln ())

  and print_fn_impl (name : string) (def : fn_def) =
    print_fn_sig ~end_with_semicolon:false name def;
    write " ";
    print_block def.body;
    writeln ()

  and print_block (block : block) =
    write "{";
    if block |> List.length <> 0 then writeln ();
    inc_indentation ();
    block
    |> List.iter (fun stmt ->
      print_stmt stmt;
      (match stmt with
       | GotoLabel _ | Comment _ -> ()
       | _ -> write ";");
      writeln ());
    dec_indentation ();
    write "}"

  and print_program (program : program) =
    write [%include_file "runtime.c"];
    program.includes
    |> StringSet.iter (fun s ->
      write "#include <";
      write s;
      write ">";
      writeln ());
    write "typedef struct {} _Unit;";
    writeln ();
    program.types
    |> StringMap.iter (fun name def ->
      let shape_name =
        match def.shape with
        | Enum _ -> Some "enum"
        | Struct _ -> Some "struct"
        | Union _ -> Some "union"
        | Fn _ -> None
        | Alias _ -> None
      in
      match shape_name with
      | Some shape_name ->
        write "typedef ";
        write shape_name;
        write " ";
        write name;
        write " ";
        write name;
        write ";";
        writeln ()
      | None -> ());
    let declared_types = ref StringMap.empty in
    let rec ensure_typedef_completed (name : string) : unit =
      match !declared_types |> StringMap.find_opt name with
      | Some BeingDeclared -> fail "recursive typedef %s" name
      | Some Completed -> ()
      | None ->
        declared_types := !declared_types |> StringMap.add name BeingDeclared;
        let def = program.types |> StringMap.find name in
        write_comment def.comment;
        (match def.shape with
         | Fn { args; result_ty } ->
           args |> List.iter ensure_type_declared;
           result_ty |> ensure_type_declared;
           write "typedef ";
           print_ty result_ty;
           write " (*";
           write name;
           write ")(";
           args
           |> List.iteri (fun i arg ->
             if i <> 0 then write ", ";
             print_ty arg);
           write ");";
           writeln ()
         | Enum variants ->
           write "enum ";
           write name;
           write " {";
           writeln ();
           inc_indentation ();
           variants
           |> StringSet.iter (fun variant ->
             write variant;
             write ",";
             writeln ());
           dec_indentation ();
           write "};";
           writeln ()
         | Struct fields ->
           fields |> StringMap.iter (fun _ field_ty -> ensure_type_completed field_ty);
           write "struct ";
           write name;
           write " {";
           writeln ();
           inc_indentation ();
           fields
           |> StringMap.iter (fun field_name field_ty ->
             print_ty field_ty;
             write " ";
             write field_name;
             write ";";
             writeln ());
           dec_indentation ();
           write "};";
           writeln ()
         | Union variants ->
           variants
           |> StringMap.iter (fun _ variant_ty -> ensure_type_completed variant_ty);
           write "union ";
           write name;
           write " {";
           writeln ();
           inc_indentation ();
           variants
           |> StringMap.iter (fun variant_name variant_ty ->
             print_ty variant_ty;
             write " ";
             write variant_name;
             write ";";
             writeln ());
           dec_indentation ();
           write "};";
           writeln ()
         | Alias ty ->
           write "typedef ";
           print_ty ty;
           write " ";
           write name;
           write ";";
           writeln ());
        declared_types := !declared_types |> StringMap.add name Completed
    and ensure_type_declared (ty : ty) : unit =
      match ty with
      | Unit -> ()
      | Raw _ -> ()
      | Named name ->
        (match (program.types |> StringMap.find name).shape with
         | Fn _ | Alias _ -> ensure_typedef_completed name
         | _ -> ())
      | Ptr pointee -> ensure_type_declared pointee
      | Void -> ()
    and ensure_type_completed (ty : ty) : unit =
      match ty with
      | Unit -> ()
      | Raw _ -> ()
      | Named name -> ensure_typedef_completed name
      | Ptr pointee -> ensure_type_declared pointee
      | Void -> ()
    in
    program.types |> StringMap.iter (fun name _def -> ensure_typedef_completed name);
    program.statics
    |> List.iter (fun (static : static) ->
      write_comment static.comment;
      print_ty static.ty;
      write " ";
      write static.name;
      write ";";
      writeln ());
    program.fns |> StringMap.iter (print_fn_sig ~end_with_semicolon:true);
    program.fns |> StringMap.iter print_fn_impl
  ;;
end
