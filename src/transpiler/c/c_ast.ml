open Std
open Kast_util

type literal =
  | L_Bool of bool
  | L_Int32 of int32
  | L_Int64 of int64
  | L_Float64 of float
  | L_Char of char
  | L_String of string

and field_initializer =
  { name : string
  ; value : pure_expr
  }

and pure_expr =
  | Pure_Native of native_expr
  | Pure_Unit
  | Pure_AddrOf of place_expr
  | Pure_Copy of place_expr
  | Pure_Literal of literal
  | Pure_Not of pure_expr
  | Pure_Equal of pure_expr * pure_expr
  | Pure_And of pure_expr * pure_expr
  | Pure_Or of pure_expr * pure_expr
  | Pure_Cast of
      { value : pure_expr
      ; target : ty
      }
  | Pure_Compound of
      { ty : ty
      ; fields : field_initializer list
      }

and expr =
  | E_Pure of pure_expr
  | E_Native of native_expr
  | E_Apply of
      { f : pure_expr
      ; args : pure_expr list
      }
  | E_Block of block

and stmt =
  | S_Native of native_expr
  | S_Comment of string
  | S_DeclareVar of
      { name : string
      ; ty : ty
      ; value : expr option
      }
  | S_Expr of expr
  | S_If of
      { cond : expr
      ; then_case : block
      ; else_case : block option
      }
  | S_Switch of
      { value : expr
      ; cases : switch_case list
      ; default : block option
      }
  | S_Assign of
      { assignee : place_expr
      ; value : expr
      }
  | S_Goto of { label : string }
  | S_GotoLabel of string
  | S_For of { body : block }
  | S_Return of expr
  | S_ReturnVoid

and switch_case =
  { value : pure_expr
  ; body : block
  }

and field =
  { name : string
  ; value : expr
  }

and place_expr =
  | P_Ident of string
  | P_Native of native_expr
  | P_Field of
      { obj : place_expr
      ; field : string
      }
  | P_Deref of pure_expr

and native_expr = native_expr_part list

and native_expr_part =
  | N_Raw of string
  | N_Interpolated of pure_expr

and ty_def_shape =
  | TD_Enum of StringSet.t
  | TD_Struct of ty StringMap.t
  | TD_Union of ty StringMap.t
  | TD_Fn of
      { args : ty list
      ; result_ty : ty
      }
  | TD_Alias of ty
  | TD_Raw of
      { def : string
      ; impl : string option
      ; need_declared : ty list
      ; need_completed : ty list
      }
  | TD_RuntimeDefined of { is_primitive : bool }

and ty_def =
  { shape : ty_def_shape
  ; comment : string option
  }

and ty =
  | T_Unit
  | T_Raw of
      { c : string
      ; is_primitive : bool
      }
  | T_Named of string
  | T_Ptr of ty
  | T_Void

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
[@@deriving ord]

and declared_state =
  | BeingDeclared
  | Completed

module Print = struct
  let indentation = ref 0
  let written_after_newline = ref false
  let inc_indentation () = indentation := !indentation + 1
  let dec_indentation () = indentation := !indentation - 1

  type _ Effect.t += GetOutput : (string -> unit) Effect.t

  let print_string s =
    let out = Effect.perform GetOutput in
    out s
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
    | P_Ident _ -> false
    | _ -> true

  and need_surround_pure_expr (expr : pure_expr) : bool =
    match expr with
    | Pure_Copy expr -> need_surround_place_expr expr
    | Pure_Literal _ -> false
    | _ -> true

  and need_surround_expr (expr : expr) : bool =
    match expr with
    | E_Pure expr -> need_surround_pure_expr expr
    | E_Block _ -> true
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
    | T_Unit -> write "Unit"
    | T_Raw { c; is_primitive = _ } -> write c
    | T_Named name -> write name
    | T_Ptr referenced ->
      print_ty referenced;
      write "*"
    | T_Void -> write "void"

  and maybe_surround surround f =
    if surround then write "(";
    f ();
    if surround then write ")"

  and print_place_expr (expr : place_expr) : unit =
    let surround = need_surround_place_expr expr in
    maybe_surround surround (fun () ->
      match expr with
      | P_Ident name -> write name
      | P_Native native -> print_native native
      | P_Field { obj; field } ->
        print_place_expr obj;
        write ".";
        write field
      | P_Deref expr ->
        write "*";
        print_pure_expr expr)

  and print_stmt (stmt : stmt) : unit =
    match stmt with
    | S_Native native -> print_native native
    | S_Comment s ->
      write "/* ";
      write s;
      write " */"
    | S_DeclareVar { name; ty; value } ->
      print_ty ty;
      write " ";
      write name;
      (match value with
       | None -> ()
       | Some value ->
         write " = ";
         print_expr value)
    | S_Expr expr -> print_expr expr
    | S_Switch { value; cases; default } ->
      write "switch (";
      print_expr value;
      write ") {";
      writeln ();
      cases
      |> List.iter (fun (case : switch_case) ->
        write "case ";
        print_pure_expr case.value;
        write ": ";
        print_block case.body;
        writeln ();
        write "break;";
        writeln ());
      (match default with
       | None -> ()
       | Some block ->
         write "default: ";
         print_block block);
      write "}"
    | S_If { cond; then_case; else_case } ->
      write "if (";
      print_expr cond;
      write ") ";
      print_block then_case;
      (match else_case with
       | Some else_case ->
         write " else ";
         print_block else_case
       | None -> ())
    | S_Assign { assignee; value } ->
      print_place_expr assignee;
      write " = ";
      print_expr value
    | S_Goto { label } ->
      write "goto ";
      write label
    | S_GotoLabel label ->
      write label;
      write ":";
      write "0;"
    | S_For { body } ->
      write "for(;;) ";
      print_block body
    | S_Return value ->
      write "return ";
      print_expr value
    | S_ReturnVoid -> write "return"

  and print_native (parts : native_expr) : unit =
    parts
    |> List.iter (function
      | N_Raw s -> write s
      | N_Interpolated expr -> print_pure_expr expr)

  and print_pure_expr (expr : pure_expr) : unit =
    let surround = need_surround_pure_expr expr in
    maybe_surround surround (fun () ->
      match expr with
      | Pure_Unit -> write "(Unit){}"
      | Pure_Native native -> print_native native
      | Pure_Not e ->
        write "!";
        print_pure_expr e
      | Pure_Literal lit ->
        write
          (match lit with
           | L_Bool x -> Bool.to_string x
           | L_Int32 x -> Int32.to_string x
           | L_Int64 x -> Int64.to_string x
           | L_Float64 x -> Float.to_string x
           | L_Char x -> make_string "%C" x
           | L_String s -> make_string "%a" String.print_debug s)
      | Pure_And (a, b) ->
        print_pure_expr a;
        write " && ";
        print_pure_expr b
      | Pure_Or (a, b) ->
        print_pure_expr a;
        write " || ";
        print_pure_expr b
      | Pure_Equal (a, b) ->
        print_pure_expr a;
        write " == ";
        print_pure_expr b
      | Pure_Copy place -> print_place_expr place
      | Pure_AddrOf place ->
        write "&";
        print_place_expr place
      | Pure_Cast { value; target } ->
        write "(";
        print_ty target;
        write ")";
        print_pure_expr value
      | Pure_Compound { ty; fields } ->
        write "(";
        print_ty ty;
        write ") {";
        inc_indentation ();
        fields
        |> List.iteri (fun i (field : field_initializer) ->
          if i = 0 then writeln ();
          write ".";
          write field.name;
          write " = ";
          print_pure_expr field.value;
          write ",";
          writeln ());
        dec_indentation ();
        write "}")

  and print_expr (expr : expr) : unit =
    let surround = need_surround_expr expr in
    maybe_surround surround (fun () ->
      match expr with
      | E_Pure expr -> print_pure_expr expr
      | E_Native native -> print_native native
      | E_Apply { f; args } ->
        print_pure_expr f;
        write "(";
        args
        |> List.iteri (fun i arg ->
          if i <> 0 then write ", ";
          print_pure_expr arg);
        write ")"
      | E_Block block -> print_block block)

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
       | S_GotoLabel _ | S_Comment _ -> ()
       | _ -> write ";");
      writeln ());
    dec_indentation ();
    write "}"

  and print_program (program : program) =
    write [%include_file "runtime.c"];
    writeln ();
    program.includes
    |> StringSet.iter (fun s ->
      write "#include <";
      write s;
      write ">";
      writeln ());
    writeln ();
    program.types
    |> StringMap.iter (fun name def ->
      let shape_name =
        match def.shape with
        | TD_Enum _ -> Some "enum"
        | TD_Struct _ -> Some "struct"
        | TD_Union _ -> Some "union"
        | TD_Fn _ -> None
        | TD_Alias _ -> None
        | TD_Raw _ -> None
        | TD_RuntimeDefined _ -> None
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
        let def =
          program.types
          |> StringMap.find_opt name
          |> Option.unwrap_or_else (fun () -> fail "type %S is not in program" name)
        in
        (match def.shape with
         | TD_RuntimeDefined _ -> ()
         | TD_Raw { def = _; impl = _; need_declared; need_completed } ->
           need_declared |> List.iter ensure_type_declared;
           need_completed |> List.iter ensure_type_completed;
           write "/*";
           need_declared
           |> List.iter (fun dep ->
             write "\nneed_declared ";
             print_ty dep);
           need_completed
           |> List.iter (fun dep ->
             write "\nneed_completed ";
             print_ty dep);
           write " */\n"
         | TD_Fn { args; result_ty } ->
           args |> List.iter ensure_type_declared;
           result_ty |> ensure_type_declared
         | TD_Enum _ -> ()
         | TD_Struct fields ->
           fields |> StringMap.iter (fun _ field_ty -> ensure_type_completed field_ty)
         | TD_Union variants ->
           variants
           |> StringMap.iter (fun _ variant_ty -> ensure_type_completed variant_ty)
         | TD_Alias ty -> ensure_type_declared ty);
        write_comment def.comment;
        (match def.shape with
         | TD_RuntimeDefined _ -> ()
         | TD_Raw { def; _ } ->
           write def;
           write ";";
           writeln ()
         | TD_Fn { args; result_ty } ->
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
         | TD_Enum variants ->
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
         | TD_Struct fields ->
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
         | TD_Union variants ->
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
         | TD_Alias ty ->
           write "typedef ";
           print_ty ty;
           write " ";
           write name;
           write ";";
           writeln ());
        declared_types := !declared_types |> StringMap.add name Completed
    and ensure_type_declared (ty : ty) : unit =
      match ty with
      | T_Unit -> ()
      | T_Raw _ -> ensure_type_completed ty
      | T_Named name ->
        (match program.types |> StringMap.find_opt name with
         | None -> fail "type doesnt exist: %s" name
         | Some { shape = TD_Fn _ | TD_Alias _ | TD_Raw _; _ } ->
           ensure_typedef_completed name
         | _ -> ())
      | T_Ptr pointee -> ensure_type_declared pointee
      | T_Void -> ()
    and ensure_type_completed (ty : ty) : unit =
      match ty with
      | T_Unit -> ()
      | T_Raw _ -> ()
      | T_Named name -> ensure_typedef_completed name
      | T_Ptr pointee -> ensure_type_declared pointee
      | T_Void -> ()
    in
    program.types |> StringMap.iter (fun name _def -> ensure_typedef_completed name);
    program.types
    |> StringMap.iter (fun _name def ->
      match def.shape with
      | TD_Raw { impl = Some impl; _ } ->
        write impl;
        write ";";
        writeln ()
      | _ -> ());
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
