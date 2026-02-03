open Std
open Kast_util
open Kast_types
open Types
module Inference = Kast_inference_base

module Error = struct
  type error =
    { msg : formatter -> unit
    ; span : span
    }

  type _ Effect.t += Error : error -> unit Effect.t

  let error : 'a. span -> ('a, formatter, unit, unit) format4 -> 'a =
    fun span format ->
    Format.kdprintf (fun msg -> Effect.perform <| Error { msg; span }) format
  ;;

  let print : formatter -> error -> unit =
    fun fmt error ->
    fprintf
      fmt
      "@{<red>Completion error:@} %t @{<dim>at %a@}"
      error.msg
      Span.print
      error.span
  ;;

  let error_context : 'a. span:span -> (unit -> 'a) -> (formatter -> unit) -> 'a =
    fun ~span f print_ctx ->
    try f () with
    | effect Error e, k ->
      Effect.perform
      <| Error
           { msg =
               (fun fmt ->
                 e.msg fmt;
                 fprintf fmt " \n";
                 print_ctx fmt)
           ; span = (if e.span |> Span.is_fake then span else e.span)
           };
      Effect.continue k ()
  ;;
end

let error = Error.error

module RecurseCache = RecurseCache.Make (Id)

module Impl = struct
  let fake_span = Span.fake "<completion>"

  let rec _unused () = ()

  and complete_value ({ var; ty } : value) =
    var |> complete_var ~name:"value" complete_value_shape;
    match ty with
    | Some ty -> complete_ty ty
    | None -> ()

  and complete_value_shape (shape : value_shape) =
    Error.error_context
      ~span:fake_span
      (fun () ->
         match shape with
         | V_Unit -> ()
         | V_Bool (_ : bool) -> ()
         | V_Int32 (_ : int32) -> ()
         | V_Int64 (_ : int64) -> ()
         | V_Float64 (_ : float) -> ()
         | V_Char (_ : Uchar.t) -> ()
         | V_Ref { mut : bool = _; place } -> complete_place place
         | V_String (_ : string) -> ()
         | V_Tuple { ty; tuple } ->
           complete_ty_tuple ty;
           complete_tuple complete_value_tuple_field tuple
         | V_Variant { label : Label.t = _; data; ty } ->
           complete_ty_variant ty;
           complete_option complete_place data
         | V_Ty ty -> complete_ty ty
         | V_Fn { ty; fn } ->
           complete_ty_fn ty;
           complete_untyped_fn fn
         | V_Generic { name; ty; fn } ->
           complete_name_shape name;
           complete_ty_generic ty;
           complete_untyped_fn fn
         | V_NativeFn { id : id = _; name : string = _; ty; impl = _ } ->
           complete_ty_fn ty
         | V_Ast _ -> ()
         | V_UnwindToken { id : id = _; result_ty } -> complete_ty result_ty
         | V_Target { name : string = _ } -> ()
         | V_ContextTy ty -> complete_context_ty ty
         | V_CompilerScope _ -> ()
         | V_Opaque { ty; value : Obj.t = _ } -> complete_ty_opaque ty
         | V_Blocked blocked -> complete_blocked_value blocked
         | V_Error -> ())
      (fun fmt -> fprintf fmt "while completing value shape %a" Value.Shape.print shape)

  and complete_context_ty ({ id : id = _; ty } : value_context_ty) = complete_ty ty

  and complete_blocked_value ({ shape; ty } : blocked_value) =
    complete_blocked_value_shape shape;
    complete_ty ty

  and complete_blocked_value_shape (shape : blocked_value_shape) =
    match shape with
    | BV_Binding binding -> complete_binding binding
    | BV_Instantiate { generic; arg } ->
      complete_blocked_value generic;
      complete_value arg
    | BV_ClaimRef ref -> complete_blocked_value ref
    | BV_FieldRef { obj_ref; member : Tuple.member = _ } -> complete_blocked_value obj_ref

  and complete_binding
        ({ id : id = _
         ; scope = _
         ; name = _
         ; span = _
         ; ty
         ; label = _
         ; mut : bool = _
         ; hygiene = _
         ; def_site = _
         } :
          binding)
    =
    complete_ty ty

  and complete_untyped_fn
        ({ id : id = _; def; captured = _; monomorphization_state = _ } :
          value_untyped_fn)
    =
    complete_maybe_compiled_fn def

  and complete_maybe_compiled_fn ({ span; compiled; on_compiled = _ } : maybe_compiled_fn)
    =
    match compiled with
    | None -> error span "Function could not be compiled"
    | Some { args; body } ->
      complete_pattern_args args;
      complete_expr body

  and complete_pattern_args ({ pattern } : pattern_args) = complete_pattern pattern

  and complete_value_tuple_field ({ place; span = _; ty_field } : value_tuple_field) =
    complete_place place;
    complete_ty_tuple_field ty_field

  and complete_place ({ id : id = _; state; ty; mut : place_mut = _ } : place) =
    complete_ty ty;
    match state with
    | Uninitialized -> ()
    | Occupied value -> complete_value value
    | MovedOut -> ()

  and complete_ty ({ var } : ty) = var |> complete_var ~name:"ty" complete_ty_shape

  and complete_ty_shape (shape : ty_shape) =
    Error.error_context
      ~span:fake_span
      (fun () ->
         match shape with
         | T_Unit -> ()
         | T_Bool -> ()
         | T_Int32 -> ()
         | T_Int64 -> ()
         | T_Float64 -> ()
         | T_String -> ()
         | T_Char -> ()
         | T_Ref { mut; referenced } ->
           complete_is_mutable mut;
           complete_ty referenced
         | T_Variant ty -> complete_ty_variant ty
         | T_Tuple ty -> complete_ty_tuple ty
         | T_Ty -> ()
         | T_Fn ty -> complete_ty_fn ty
         | T_Generic ty -> complete_ty_generic ty
         | T_Ast -> ()
         | T_UnwindToken ty -> complete_ty_unwind_token ty
         | T_Target -> ()
         | T_ContextTy -> ()
         | T_CompilerScope -> ()
         | T_Opaque ty -> complete_ty_opaque ty
         | T_Blocked blocked -> complete_blocked_value blocked
         | T_Error -> ())
      (fun fmt -> fprintf fmt "while completing ty shape %a" Ty.Shape.print shape)

  and complete_ty_unwind_token ({ result } : ty_unwind_token) = complete_ty result

  and complete_is_mutable ({ var } : is_mutable) =
    var |> complete_var ~name:"is_mutable" (fun (_ : bool) -> ())

  and complete_ty_tuple ({ name; tuple } : ty_tuple) =
    complete_optional_name name;
    complete_tuple complete_ty_tuple_field tuple

  and complete_ty_variant ({ name; variants } : ty_variant) =
    complete_optional_name name;
    complete_row ~name:"ty variant" complete_ty_variant_data variants

  and complete_ty_variant_data ({ data } : ty_variant_data) =
    complete_option complete_ty data

  and complete_ty_fn ({ args; result } : ty_fn) =
    complete_ty_args args;
    complete_ty result

  and complete_ty_args ({ ty } : ty_args) = complete_ty ty

  and complete_ty_generic ({ args; result } : ty_generic) =
    complete_pattern_args args;
    complete_ty result

  and complete_ty_opaque ({ name } : ty_opaque) = complete_name name

  and complete_ty_tuple_field ({ ty; symbol = _; label = _ } : ty_tuple_field) =
    complete_ty ty

  and complete_name ({ var } : name) =
    var |> complete_var ~name:"name" complete_name_shape

  and complete_optional_name ({ var } : optional_name) =
    var |> complete_var ~name:"optional name" (complete_option complete_name_shape)

  and complete_name_shape (shape : name_shape) =
    match shape with
    | Simple part -> complete_name_part part
    | Concat (a, b) ->
      complete_name_shape a;
      complete_name_part b
    | Instantiation { generic; arg } ->
      complete_value generic;
      complete_value arg

  and complete_name_part (part : name_part) =
    match part with
    | Uri _ -> ()
    | Str _ -> ()
    | Symbol _ -> ()

  and complete_pattern ({ shape; data } : pattern) =
    Error.error_context
      ~span:data.span
      (fun () ->
         complete_pattern_shape shape;
         complete_ir_data data)
      (fun fmt -> fprintf fmt "while completing pattern at %a" Span.print data.span)

  and complete_pattern_shape (shape : pattern_shape) =
    match shape with
    | P_Placeholder -> ()
    | P_Ref referenced -> complete_pattern referenced
    | P_Unit -> ()
    | P_Binding { bind_mode = _; binding } -> complete_binding binding
    | P_Tuple { guaranteed_anonymous : bool = _; parts } ->
      parts |> List.iter (complete_tuple_part_of complete_pattern)
    | P_Variant { label = _; label_span = _; value } ->
      complete_option complete_pattern value
    | P_Error -> ()

  and complete_expr ({ shape; data } : expr) =
    Error.error_context
      ~span:data.span
      (fun () ->
         complete_expr_shape shape;
         complete_ir_data data)
      (fun fmt -> fprintf fmt "while completing expr at %a" Span.print data.span)

  and complete_expr_shape (shape : expr_shape) =
    match shape with
    | E_Constant { id = _; value } -> complete_value value
    | E_Ref { mut : bool = _; place } -> complete_place_expr place
    | E_Claim place -> complete_place_expr place
    | E_Then { list } -> list |> List.iter complete_expr
    | E_Stmt { expr } -> complete_expr expr
    | E_Scope { expr } -> complete_expr expr
    | E_Fn { ty; def } ->
      complete_ty_fn ty;
      complete_maybe_compiled_fn def
    | E_Generic { ty; def } ->
      complete_ty_generic ty;
      complete_maybe_compiled_fn def
    | E_Tuple { guaranteed_anonymous : bool = _; parts } ->
      parts |> List.iter (complete_tuple_part_of complete_expr)
    | E_Variant { label = _; label_span = _; value } ->
      complete_option complete_expr value
    | E_Apply { f; arg } ->
      complete_expr f;
      complete_expr arg
    | E_InstantiateGeneric { generic; arg } ->
      complete_expr generic;
      complete_expr arg
    | E_Assign { assignee; value } ->
      complete_assignee_expr assignee;
      complete_place_expr value
    | E_Ty expr -> complete_ty_expr expr
    | E_Newtype expr -> complete_ty_expr expr
    | E_Native { id : id = _; expr : string = _ } -> ()
    | E_Module { def; bindings } ->
      complete_expr def;
      bindings |> List.iter complete_binding
    | E_UseDotStar { used; bindings } ->
      complete_expr used;
      bindings |> List.iter complete_binding
    | E_If { cond; then_case; else_case } ->
      complete_expr cond;
      complete_expr then_case;
      complete_expr else_case
    | E_And { lhs; rhs } ->
      complete_expr lhs;
      complete_expr rhs
    | E_Or { lhs; rhs } ->
      complete_expr lhs;
      complete_expr rhs
    | E_Match { value; branches } ->
      complete_place_expr value;
      branches
      |> List.iter (fun { pattern; body } ->
        complete_pattern pattern;
        complete_expr body)
    | E_QuoteAst expr -> complete_expr_quote_ast expr
    | E_Loop { body } -> complete_expr body
    | E_Unwindable { token; body } ->
      complete_pattern token;
      complete_expr body
    | E_Unwind { token; value } ->
      complete_expr token;
      complete_expr value
    | E_InjectContext { context_ty; value } ->
      complete_context_ty context_ty;
      complete_expr value
    | E_CurrentContext { context_ty } -> complete_context_ty context_ty
    | E_ImplCast { value; target; impl } ->
      complete_expr value;
      complete_value target;
      complete_expr impl
    | E_Cast { value; target } ->
      complete_expr value;
      complete_value target
    | E_TargetDependent { branches; captured = _; interpreter_branch = _ } ->
      branches
      |> List.iter (fun { cond; body } ->
        complete_expr cond;
        complete_expr body)
    | E_Error -> ()

  and complete_expr_quote_ast (expr : expr_quote_ast) =
    match expr with
    | Simple { ast = _; def_site = _ } -> ()
    | Complex { rule = _; root; def_site = _ } ->
      let rec complete_group ({ rule = _; children; span = _ } : expr_quote_ast_group) =
        children
        |> complete_tuple (fun (child : expr_quote_ast_child) ->
          match child with
          | Group group -> complete_group group
          | Ast expr -> complete_expr expr)
      in
      complete_group root

  and complete_place_expr ({ shape; mut; data } : place_expr) =
    Error.error_context
      ~span:data.span
      (fun () ->
         complete_place_expr_shape shape;
         complete_is_mutable mut;
         complete_ir_data data)
      (fun fmt -> fprintf fmt "while completing place expr at %a" Span.print data.span)

  and complete_place_expr_shape (shape : place_expr_shape) =
    match shape with
    | PE_Binding binding -> complete_binding binding
    | PE_Field { obj; field; field_span = _ } ->
      complete_place_expr obj;
      (match field with
       | Index (_ : int) -> ()
       | Name (_ : Label.t) -> ()
       | Expr e -> complete_expr e)
    | PE_Deref expr -> complete_expr expr
    | PE_Temp expr -> complete_expr expr
    | PE_Error -> ()

  and complete_assignee_expr ({ shape; data } : assignee_expr) =
    Error.error_context
      ~span:data.span
      (fun () ->
         complete_assignee_expr_shape shape;
         complete_ir_data data)
      (fun fmt -> fprintf fmt "while completing assignee expr at %a" Span.print data.span)

  and complete_assignee_expr_shape (shape : assignee_expr_shape) =
    match shape with
    | A_Placeholder -> ()
    | A_Unit -> ()
    | A_Tuple { guaranteed_anonymous : bool = _; parts } ->
      parts |> List.iter (complete_tuple_part_of complete_assignee_expr)
    | A_Place expr -> complete_place_expr expr
    | A_Let pattern -> complete_pattern pattern
    | A_Error -> ()

  and complete_ty_expr ({ compiled_shape; on_compiled = _; data } : ty_expr) =
    Error.error_context
      ~span:data.span
      (fun () ->
         (match compiled_shape with
          | None -> error data.span "ty expr not compiled"
          | Some shape -> complete_ty_expr_shape shape);
         complete_ir_data data)
      (fun fmt -> fprintf fmt "while completing ty expr at %a" Span.print data.span)

  and complete_ty_expr_shape (shape : ty_expr_shape) =
    match shape with
    | TE_Unit -> ()
    | TE_Ref { mut; referenced } ->
      complete_is_mutable mut;
      complete_ty_expr referenced
    | TE_Fn { arg; result } ->
      complete_ty_expr arg;
      complete_ty_expr result
    | TE_Expr expr -> complete_expr expr
    | TE_Tuple { guaranteed_anonymous : bool = _; parts } ->
      parts |> List.iter (complete_tuple_part_of complete_ty_expr)
    | TE_Variant { variants } ->
      variants
      |> List.iter
           (fun ({ label = _; label_span = _; value } : ty_expr_variant_variant) ->
              complete_option complete_ty_expr value)
    | TE_Union { elements } -> elements |> List.iter complete_ty_expr
    | TE_Error -> ()

  and complete_ir_data
        ({ span = _; ty; evaled = _; included_file = _; id = _; compiler_scope = _ } :
          ir_data)
    =
    complete_ty ty

  and complete_tuple_part_of : 'a. ('a -> unit) -> 'a tuple_part_of -> unit =
    fun (type a) (complete_value : a -> unit) part ->
    match part with
    | Field { label_span = _; label = _; field } -> complete_value field
    | Unpack value -> complete_value value

  and complete_row : 'a. name:string -> ('a -> unit) -> ('a, var_scope) Row.t -> unit =
    fun (type a) ~name (complete_data : a -> unit) ({ var } : (a, var_scope) Row.t) ->
    var |> complete_var ~name:(name ^ " row") (complete_row_shape ~name complete_data)

  and complete_row_shape
    : 'a. name:string -> ('a -> unit) -> ('a, var_scope) Row.shape -> unit
    =
    fun (type a) ~name (complete_data : a -> unit) (shape : (a, var_scope) Row.shape) ->
    match shape with
    | R_Empty -> ()
    | R_Cons { label : Label.t = _; value; rest } ->
      complete_data value;
      complete_row ~name complete_data rest
    | R_Error -> ()

  and complete_tuple : 'a. ('a -> unit) -> 'a tuple -> unit =
    fun (type a) (complete_field : a -> unit) (tuple : a tuple) ->
    tuple |> Tuple.iter (fun _member field -> complete_field field)

  and complete_option : 'a. ('a -> unit) -> 'a option -> unit =
    fun (type a) (complete_value : a -> unit) (opt : a option) ->
    match opt with
    | Some value -> complete_value value
    | None -> ()

  and complete_var : 'a. name:string -> ('a -> unit) -> 'a var -> unit =
    fun (type a) ~name (complete_inferred : a -> unit) (var : a var) ->
    let cache = RecurseCache.get () in
    let id = var |> Inference.Var.recurse_id in
    if cache |> RecurseCache.is_visited id
    then ()
    else (
      cache |> RecurseCache.enter id;
      match var |> Inference.Var.inferred_opt with
      | Some inferred -> complete_inferred inferred
      | None ->
        let span = Span.fake "<completion>" in
        error span "%s not inferred" name)
  ;;
end

let enable = ref true

let with_cache f arg =
  if !enable then RecurseCache.with_cache (RecurseCache.create ()) (fun () -> f arg)
;;

let complete_value value = with_cache Impl.complete_value value

let complete_compiled : 'a. 'a compiled_kind -> 'a -> unit =
  fun (type a) (kind : a compiled_kind) ->
  with_cache (fun (compiled : a) ->
    match kind with
    | Assignee -> Impl.complete_assignee_expr compiled
    | Expr -> Impl.complete_expr compiled
    | PlaceExpr -> Impl.complete_place_expr compiled
    | TyExpr -> Impl.complete_ty_expr compiled
    | Pattern -> Impl.complete_pattern compiled)
;;
