open Std
open Kast_util
open Print
open Types
module Inference = Kast_inference_base

module VarScopeImpl = struct
  type t = var_scope

  let root () = None

  let rec unite (a : t) (b : t) : t =
    (* TODO more effecient algorithm? *)
    match (a, b) with
    | None, None | None, Some _ | Some _, None -> None
    | Some a, Some b ->
        let diff = Int.compare a.depth b.depth in
        if diff < 0 then unite (Some a) b.parent
        else if diff > 0 then unite a.parent (Some b)
        else if Id.equal a.id b.id then Some a
        else None

  let equal =
    Option.equal (fun (a : interpreter_scope) (b : interpreter_scope) ->
        Id.equal a.id b.id)

  let compare =
    Option.compare (fun (a : interpreter_scope) (b : interpreter_scope) ->
        Id.compare a.id b.id)

  let rec contains (child : t) (ansestor : t) : bool =
    (* TODO more effecient? *)
    match (child, ansestor) with
    | _, None -> true
    | None, Some _ -> false
    | Some child, Some ansestor ->
        let diff = Int.compare child.depth ansestor.depth in
        if diff < 0 then false
        else if diff > 0 then contains child.parent (Some ansestor)
        else if Id.equal child.id ansestor.id then true
        else false

  let deepest (a : t) (b : t) : t =
    match (a, b) with
    | None, None -> None
    | None, Some scope | Some scope, None -> Some scope
    | Some a, Some b -> if a.depth > b.depth then Some a else Some b
end

module VarScope = struct
  include VarScopeImpl

  let of_bool : bool -> var_scope = fun _ -> root ()

  let rec _unused () = ()
  and of_ty : ty -> var_scope = fun { var } -> Inference.Var.scope var

  and of_ty_shape : ty_shape -> var_scope =
   fun shape ->
    match shape with
    | T_Unit | T_Bool | T_Int32 | T_Int64 | T_Float64 | T_String | T_Char ->
        root ()
    | T_Ref x -> of_ty_ref x
    | T_Variant x -> of_ty_variant x
    | T_Tuple x -> of_ty_tuple x
    | T_Ty -> root ()
    | T_Fn x -> of_ty_fn x
    | T_Generic x -> of_ty_generic x
    | T_Ast -> root ()
    | T_UnwindToken x -> of_ty_unwind_token x
    | T_Target -> root ()
    | T_ContextTy -> root ()
    | T_CompilerScope -> root ()
    | T_Opaque x -> of_ty_opaque x
    | T_Blocked x -> of_blocked_value x
    | T_Error -> root ()

  and of_ty_variant_data : ty_variant_data -> var_scope =
   fun { data } ->
    match data with
    | None -> root ()
    | Some data -> of_ty data

  and of_ty_unwind_token : ty_unwind_token -> var_scope =
   fun { result } -> of_ty result

  and of_ty_fn : ty_fn -> var_scope =
   fun { arg; result } -> deepest (of_ty arg) (of_ty result)

  and of_ty_generic : ty_generic -> var_scope =
   fun { arg; result } -> deepest (of_ty arg.data.ty) (of_ty result)

  and of_ty_opaque : ty_opaque -> var_scope = fun { name } -> of_name name

  and of_ty_variant : ty_variant -> var_scope =
   fun { name; variants } ->
    deepest (of_optional_name name) (Row.scope (module VarScopeImpl) variants)

  and of_ty_tuple : ty_tuple -> var_scope =
   fun { name; tuple } ->
    tuple |> Tuple.to_seq
    |> Seq.fold_left
         (fun acc (_member, field) -> deepest acc (of_ty_tuple_field field))
         (of_optional_name name)

  and of_ty_tuple_field : ty_tuple_field -> var_scope =
   fun { ty; label = _ } -> of_ty ty

  and of_ty_ref : ty_ref -> var_scope =
   fun { mut; referenced } -> deepest (of_is_mutable mut) (of_ty referenced)

  and of_is_mutable : is_mutable -> var_scope =
   fun { var } -> Inference.Var.scope var

  and of_optional_name : optional_name -> var_scope =
   fun { var } -> Inference.Var.scope var

  and of_name : name -> var_scope = fun { var } -> Inference.Var.scope var

  and of_name_shape : name_shape -> var_scope = function
    | Simple part -> of_name_part part
    | Concat (a, b) -> deepest (of_name_shape a) (of_name_part b)
    | Instantiation x -> of_name_instantiation x

  and of_name_part : name_part -> var_scope = function
    | Uri _ -> root ()
    | Str _ -> root ()
    | Symbol _ -> root ()

  and of_name_instantiation : name_instantiation -> var_scope =
   fun { generic; arg } -> deepest (of_value generic) (of_value arg)

  and of_blocked_value : blocked_value -> var_scope =
   fun { shape; ty } -> deepest (of_blocked_value_shape shape) (of_ty ty)

  and of_blocked_value_shape : blocked_value_shape -> var_scope = function
    | BV_Binding binding -> binding.scope
    | BV_Instantiate x -> of_blocked_value_instantiate x
    | BV_ClaimRef x -> of_blocked_value x
    | BV_FieldRef x -> of_blocked_value_field_ref x

  and of_blocked_value_instantiate : blocked_value_instantiate -> var_scope =
   fun { generic; arg } -> deepest (of_blocked_value generic) (of_value arg)

  and of_blocked_value_field_ref : blocked_value_field_ref -> var_scope =
   fun { obj_ref; member : Tuple.member = _ } -> of_blocked_value obj_ref

  and of_value : value -> var_scope =
   fun { var; ty } -> deepest (Inference.Var.scope var) (of_ty ty)

  and of_value_shape : value_shape -> var_scope = function
    | V_Unit -> root ()
    | V_Bool (_ : bool) -> root ()
    | V_Int32 (_ : int32) -> root ()
    | V_Int64 (_ : int64) -> root ()
    | V_Float64 (_ : float) -> root ()
    | V_Char (_ : char) -> root ()
    | V_Ref x -> of_value_ref x
    | V_String (_ : string) -> root ()
    | V_Tuple x -> of_value_tuple x
    | V_Variant x -> of_value_variant x
    | V_Ty ty -> of_ty ty
    | V_Fn x -> of_value_fn x
    | V_Generic x -> of_value_generic x
    | V_NativeFn x -> of_value_native_fn x
    | V_Ast (_ : Ast.t) -> root ()
    | V_UnwindToken x -> of_value_unwind_token x
    | V_Target x -> of_value_target x
    | V_ContextTy x -> of_value_context_ty x
    | V_CompilerScope (_ : compiler_scope) -> root ()
    | V_Opaque x -> of_value_opaque x
    | V_Blocked blocked -> of_blocked_value blocked
    | V_Error -> root ()

  and of_value_ref : value_ref -> var_scope =
   fun { mut : bool = _; place } ->
    (*  TODO check if place can be ignored *)
    of_place place

  and of_value_tuple : value_tuple -> var_scope =
   fun { tuple; ty } ->
    tuple |> Tuple.to_seq
    |> Seq.fold_left
         (fun acc (_member, field) -> deepest acc (of_value_tuple_field field))
         (of_ty_tuple ty)

  and of_value_tuple_field : value_tuple_field -> var_scope =
   fun { place; span : Span.t = _; ty_field } ->
    deepest (of_place place) (of_ty_tuple_field ty_field)

  and of_value_variant : value_variant -> var_scope =
   fun { label = _; data; ty } ->
    deepest (of_option of_place data) (of_ty_variant ty)

  and of_value_fn : value_fn -> var_scope =
   fun { ty; fn : value_untyped_fn = _ } -> of_ty_fn ty

  and of_value_generic : value_generic -> var_scope =
   fun { id = _; name; fn : value_untyped_fn = _; ty } ->
    deepest (of_name_shape name) (of_ty_generic ty)

  and of_value_native_fn : value_native_fn -> var_scope =
   fun { id = _; name : string = _; ty; impl = _ } -> of_ty_fn ty

  and of_value_unwind_token : value_unwind_token -> var_scope =
   fun { id = _; result_ty } -> of_ty result_ty

  and of_value_target : value_target -> var_scope =
   fun { name : string = _ } -> root ()

  and of_value_context_ty : value_context_ty -> var_scope =
   fun { id = _; ty } -> of_ty ty

  and of_value_opaque : value_opaque -> var_scope =
   fun { ty; value : Obj.t = _ } -> of_ty_opaque ty

  and of_place : place -> var_scope =
   fun { id = _; ty; mut : place_mut = _; state } ->
    deepest (of_ty ty) (of_place_state state)

  and of_place_state : place_state -> var_scope = function
    | Uninitialized -> root ()
    | MovedOut -> root ()
    | Occupied value -> of_value value

  and of_option : 'a. ('a -> var_scope) -> 'a option -> var_scope =
   fun of_x -> function
    | None -> root ()
    | Some x -> of_x x
end

type ctx = { mutable normalized_bindings : binding Id.Map.t }
type _ Effect.t += GetCtx : ctx Effect.t

let sub_ty : (span:span -> state:sub_state -> ty -> ty) option ref = ref None

let with_ctx f =
  let ctx = { normalized_bindings = Id.Map.empty } in
  try f () with effect GetCtx, k -> Effect.continue k ctx

let unite_with_ctx (f : 'a Inference.unite) ~span a b =
  with_ctx (fun () -> f ~span a b)

let error = Inference.Error.error

module Impl = struct
  let rec _unused () = ()

  and unite_ty_shape : ty_shape Inference.unite =
   fun ~span a b ->
    let fail () : ty_shape =
      error span "ty_shape %a != %a" print_ty_shape a print_ty_shape b;
      T_Error
    in
    match (a, b) with
    | T_Error, smth | smth, T_Error -> smth
    | T_Unit, T_Unit -> T_Unit
    | T_Unit, _ -> fail ()
    | T_Bool, T_Bool -> T_Bool
    | T_Bool, _ -> fail ()
    | T_Int32, T_Int32 -> T_Int32
    | T_Int32, _ -> fail ()
    | T_Int64, T_Int64 -> T_Int64
    | T_Int64, _ -> fail ()
    | T_Float64, T_Float64 -> T_Float64
    | T_Float64, _ -> fail ()
    | T_Char, T_Char -> T_Char
    | T_Char, _ -> fail ()
    | T_String, T_String -> T_String
    | T_String, _ -> fail ()
    | T_Ref a, T_Ref b -> T_Ref (unite_ty_ref ~span a b)
    | T_Ref _, _ -> fail ()
    | T_Tuple a, T_Tuple b -> T_Tuple (unite_ty_tuple ~span a b)
    | T_Tuple _, _ -> fail ()
    | ( T_Variant { name = name_a; variants = a },
        T_Variant { name = name_b; variants = b } ) ->
        T_Variant
          {
            name = unite_optional_name ~span name_a name_b;
            variants =
              Row.unite
                (module VarScopeImpl)
                VarScope.of_ty_variant_data ~span unite_ty_variant_data a b;
          }
    | T_Variant _, _ -> fail ()
    | T_Ty, T_Ty -> T_Ty
    | T_Ty, _ -> fail ()
    | T_Ast, T_Ast -> T_Ast
    | T_Ast, _ -> fail ()
    | T_UnwindToken { result = a }, T_UnwindToken { result = b } ->
        T_UnwindToken { result = unite_ty ~span a b }
    | T_UnwindToken _, _ -> fail ()
    | T_Fn a, T_Fn b -> T_Fn (unite_ty_fn ~span a b)
    | T_Fn _, _ -> fail ()
    | T_Generic a, T_Generic b -> T_Generic (unite_ty_generic ~span a b)
    | T_Generic _, _ -> fail ()
    | T_Target, T_Target -> T_Target
    | T_Target, _ -> fail ()
    | T_ContextTy, T_ContextTy -> T_ContextTy
    | T_ContextTy, _ -> fail ()
    | T_CompilerScope, T_CompilerScope -> T_CompilerScope
    | T_CompilerScope, _ -> fail ()
    | T_Opaque { name = name_a }, T_Opaque { name = name_b } ->
        T_Opaque { name = unite_name ~span name_a name_b }
    | T_Opaque _, _ -> fail ()
    | T_Blocked a, T_Blocked b -> T_Blocked (unite_blocked_value ~span a b)
    | T_Blocked _, _ -> fail ()

  and unite_blocked_value : blocked_value Inference.unite =
   fun ~span a b ->
    {
      shape = unite_blocked_value_shape ~span a.shape b.shape;
      ty = unite_ty ~span a.ty b.ty;
    }

  and unite_blocked_value_shape : blocked_value_shape Inference.unite =
   fun ~span a b ->
    let fail () : blocked_value_shape =
      error span "blocked_value_shape %a != %a" print_blocked_value_shape a
        print_blocked_value_shape b;
      a
    in
    match (a, b) with
    | BV_Binding a, BV_Binding b ->
        let ctx = Effect.perform GetCtx in
        let normalize (binding : binding) =
          match ctx.normalized_bindings |> Id.Map.find_opt binding.id with
          | Some normalized -> normalized
          | None -> binding
        in
        let a = normalize a in
        let b = normalize b in
        if not (Id.equal a.id b.id) then fail () else BV_Binding a
    | BV_Binding _, _ -> fail ()
    | ( BV_Instantiate { generic = generic_a; arg = arg_a },
        BV_Instantiate { generic = generic_b; arg = arg_b } ) ->
        BV_Instantiate
          {
            generic = unite_blocked_value ~span generic_a generic_b;
            arg = unite_value ~span arg_a arg_b;
          }
    | BV_Instantiate _, _ -> fail ()
    | ( BV_FieldRef { obj_ref = obj_a; member = member_a },
        BV_FieldRef { obj_ref = obj_b; member = member_b } )
      when Tuple.Member.equal member_a member_b ->
        BV_FieldRef
          { obj_ref = unite_blocked_value ~span obj_a obj_b; member = member_a }
    | BV_FieldRef _, _ -> fail ()
    | BV_ClaimRef a, BV_ClaimRef b ->
        BV_ClaimRef (unite_blocked_value ~span a b)
    | BV_ClaimRef _, _ -> fail ()

  and unite_ty_generic : ty_generic Inference.unite =
   fun ~span ({ arg = arg_a; result = result_a } as a)
       ({ arg = arg_b; result = result_b } as b) ->
    let result : ty_generic =
      let common_scope =
        VarScopeImpl.unite (VarScope.of_ty_generic a) (VarScope.of_ty_generic b)
      in

      let bindings_ab = unite_pattern ~span arg_a arg_b in
      let sub_ty = Option.get !sub_ty in

      let sub_scope_id = Id.gen () in
      let sub_with (bindings : (binding * binding) list) ty =
        let state : Types.interpreter_state =
          let locals : Types.interpreter_locals =
            {
              by_symbol =
                bindings
                |> List.map (fun (a, b) : (symbol * interpreter_local) ->
                    let binding =
                      V_Blocked { shape = BV_Binding b; ty = b.ty }
                      |> inferred_value ~span
                    in

                    let binding = init_place ~mut:Inherit binding in

                    let binding : Types.interpreter_local =
                      {
                        place = binding;
                        ty_field = { ty = binding.ty; label = None };
                      }
                    in
                    (a.name, binding))
                |> SymbolMap.of_list;
            }
          in
          {
            scope =
              {
                id = sub_scope_id;
                depth = 0;
                span;
                parent = None;
                recursive = false;
                locals;
                closed = false;
                on_update = [];
              };
            (* TODO only scope is needed, change Substitute_bindings *)
            natives = { by_name = StringMap.empty };
            current_fn_natives = Hashtbl.create 0;
            contexts = Id.Map.empty;
            instantiated_generics = { map = Id.Map.empty };
            cast_impls =
              { map = Types.ValueMap.empty; as_module = Types.ValueMap.empty };
            current_name = Simple (Str "<unused>");
          }
        in
        sub_ty ~span
          ~state:{ interpreter = state; target_scope = common_scope }
          ty
      in

      let bindings_ba = bindings_ab |> List.map (fun (a, b) -> (b, a)) in

      (* println "unite_generic_ty at %a" Span.print span; *)
      let _ : ty = unite_ty ~span (result_a |> sub_with bindings_ab) result_b in
      let _ : ty = unite_ty ~span (result_b |> sub_with bindings_ba) result_a in

      { arg = arg_a; result = result_a }
    in
    result

  and unite_pattern :
      span:span -> pattern -> pattern -> (binding * binding) list =
   fun ~span a b ->
    let fail () =
      let print_pattern =
        print_pattern ~options:{ spans = false; types = false }
      in
      error span "patterns can't be united: %a and %a" print_pattern a
        print_pattern b;
      []
    in
    let _ : ty = unite_ty ~span a.data.ty b.data.ty in
    match (a.shape, b.shape) with
    | P_Placeholder, P_Placeholder -> []
    | P_Placeholder, _ -> fail ()
    | P_Unit, P_Unit -> []
    | P_Unit, _ -> fail ()
    | P_Ref a, P_Ref b -> unite_pattern ~span a b
    | P_Ref _, _ -> fail ()
    | ( P_Binding { by_ref = by_ref_a; binding = a },
        P_Binding { by_ref = by_ref_b; binding = b } )
      when Bool.equal by_ref_a by_ref_b ->
        [ (a, b) ]
    | P_Binding _, _ -> fail ()
    | P_Tuple { parts = parts_a }, P_Tuple { parts = parts_b } ->
        let rec check_parts (a : pattern tuple_part_of list)
            (b : pattern tuple_part_of list) : (binding * binding) list =
          match (a, b) with
          | [], [] -> []
          | head_a :: tail_a, head_b :: tail_b ->
              let head_bindings =
                match (head_a, head_b) with
                | Unpack a, Unpack b -> unite_pattern ~span a b
                | Unpack _, _ -> fail ()
                | ( Field { label = label_a; label_span = _; field = field_a },
                    Field { label = label_b; label_span = _; field = field_b } )
                  ->
                    (match (label_a, label_b) with
                    | None, None -> ()
                    | Some a, Some b ->
                        let _ : Label.t = Label.unite a b in
                        ()
                    | Some _, None | None, Some _ ->
                        let _ : _ list = fail () in
                        ());
                    unite_pattern ~span field_a field_b
                | Field _, _ -> fail ()
              in
              let tail_bindings = check_parts tail_a tail_b in
              head_bindings @ tail_bindings
          | [], _ | _, [] -> fail ()
        in
        check_parts parts_a parts_b
    | P_Tuple _, _ -> fail ()
    | ( P_Variant { label = label_a; label_span = _; value = value_a },
        P_Variant { label = label_b; label_span = _; value = value_b } ) -> (
        let _ : Label.t = Label.unite label_a label_b in
        match (value_a, value_b) with
        | None, None -> []
        | Some a, Some b -> unite_pattern ~span a b
        | Some _, None | None, Some _ -> fail ())
    | P_Variant _, _ -> fail ()
    | P_Error, P_Error -> []
    | P_Error, _ -> fail ()

  and unite_ty_ref : ty_ref Inference.unite =
   fun ~span { mut = mut_a; referenced = ref_a }
       { mut = mut_b; referenced = ref_b } ->
    {
      mut = unite_is_mutable ~span mut_a mut_b;
      referenced = unite_ty ~span ref_a ref_b;
    }

  and unite_is_mutable : is_mutable Inference.unite =
   fun ~span { var = a } { var = b } ->
    { var = Inference.Var.unite unite_bool VarScopeImpl.unite ~span a b }

  and unite_bool : bool Inference.unite =
   fun ~span a b ->
    if a = b then a
    else (
      error span "bool unite failed";
      a)

  and unite_ty_tuple : ty_tuple Inference.unite =
   fun ~span ({ name = name_a; tuple = a } as tuple_a)
       ({ name = name_b; tuple = b } as tuple_b) ->
    try
      {
        name = unite_optional_name ~span name_a name_b;
        tuple =
          Tuple.zip_order_a a b
          |> Tuple.map (fun (a, b) -> unite_ty_tuple_field ~span a b);
      }
    with Invalid_argument _ ->
      error span "ty_tuple %a != %a" print_ty_tuple tuple_a print_ty_tuple
        tuple_b;
      tuple_a

  and unite_ty_tuple_field =
   fun ~span (a : Types.ty_tuple_field) (b : Types.ty_tuple_field) :
       Types.ty_tuple_field ->
    {
      ty = unite_ty ~span a.ty b.ty;
      label = unite_option (fun ~span:_ -> Label.unite) ~span a.label b.label;
    }

  and unite_ty_variant_data =
   fun ~span ({ data = a } : Types.ty_variant_data)
       ({ data = b } : Types.ty_variant_data) : Types.ty_variant_data ->
    {
      data =
        (match (a, b) with
        | None, None -> None
        | Some ty, None | None, Some ty ->
            error span "data & not data mismatch in variant";
            Some ty
        | Some a, Some b -> Some (unite_ty ~span a b));
    }

  and unite_ty_fn : ty_fn Inference.unite =
   fun ~span a b ->
    {
      arg = unite_ty ~span a.arg b.arg;
      result = unite_ty ~span a.result b.result;
    }

  and unite_ty : ty Inference.unite =
   fun ~span { var = a } { var = b } ->
    { var = Inference.Var.unite unite_ty_shape VarScopeImpl.unite ~span a b }

  and unite_place : place Inference.unite =
   fun ~span place_a place_b ->
    let get_value (place : place) : value =
      match place.state with
      | Uninitialized ->
          error span "uninitialized place";
          V_Error |> inferred_value ~span
      | Occupied value -> value
      | MovedOut ->
          error span "moved out place";
          V_Error |> inferred_value ~span
    in
    let a = get_value place_a in
    let b = get_value place_b in
    init_place
      ~mut:(unite_place_mut ~span place_a.mut place_b.mut)
      (unite_value ~span a b)

  and unite_place_mut : place_mut Inference.unite =
   fun ~span a b ->
    let fail () =
      error span "different mutabilities";
      a
    in
    match (a, b) with
    | Inherit, m | m, Inherit -> m
    | Immutable, Immutable -> Immutable
    | Immutable, _ -> fail ()
    | Mutable, Mutable -> Mutable
    | Mutable, _ -> fail ()
  (* | Inherit, Inherit -> Inherit
  | Inherit, _ -> fail () *)

  and unite_value_shape : value_shape Inference.unite =
   fun ~span a b ->
    let fail () : value_shape =
      error span "value_shape %a != %a" print_value_shape a print_value_shape b;
      V_Error
    in
    match (a, b) with
    | V_Error, smth | smth, V_Error -> smth
    | V_Ty ty, V_Blocked b | V_Blocked b, V_Ty ty ->
        V_Ty (unite_ty ~span ty (T_Blocked b |> inferred_ty ~span))
    | V_Unit, V_Unit -> V_Unit
    | V_Unit, _ -> fail ()
    | V_Bool a, V_Bool b when a = b -> V_Bool a
    | V_Bool _, _ -> fail ()
    | V_Int32 a, V_Int32 b when a = b -> V_Int32 a
    | V_Int32 _, _ -> fail ()
    | V_Int64 a, V_Int64 b when a = b -> V_Int64 a
    | V_Int64 _, _ -> fail ()
    | V_Float64 a, V_Float64 b when a = b -> V_Float64 a
    | V_Float64 _, _ -> fail ()
    | V_Char a, V_Char b when a = b -> V_Char a
    | V_Char _, _ -> fail ()
    | V_String a, V_String b when a = b -> V_String a
    | V_String _, _ -> fail ()
    | V_Ref a, V_Ref b when Repr.equal a b -> V_Ref a
    | V_Ref _, _ -> fail ()
    | V_Tuple { ty = ty_a; tuple = a }, V_Tuple { ty = ty_b; tuple = b } ->
        V_Tuple
          {
            ty = unite_ty_tuple ~span ty_a ty_b;
            tuple =
              Tuple.zip_order_a a b
              |> Tuple.map
                   (fun
                     ((a, b) :
                       Types.value_tuple_field * Types.value_tuple_field)
                     :
                     Types.value_tuple_field
                   ->
                     {
                       place = unite_place ~span a.place b.place;
                       span = a.span;
                       ty_field =
                         unite_ty_tuple_field ~span a.ty_field b.ty_field;
                     });
          }
    | V_Tuple _, _ -> fail ()
    | V_Variant _, _ -> fail () (* TODO *)
    | V_Ty a, V_Ty b -> V_Ty (unite_ty ~span a b)
    | V_Ty _, _ -> fail ()
    | V_Fn a, V_Fn b when a.fn.id = b.fn.id ->
        V_Fn { ty = unite_ty_fn ~span a.ty b.ty; fn = a.fn }
    | V_Fn _, _ -> fail ()
    | V_Generic a, V_Generic b when a.id = b.id -> V_Generic a
    | V_Generic _, _ -> fail ()
    | V_NativeFn _, _ -> fail () (* TODO *)
    | V_Ast _, _ -> fail ()
    | ( V_UnwindToken { result_ty = ty_a; id = id_a },
        V_UnwindToken { result_ty = ty_b; id = id_b } )
      when id_a = id_b ->
        V_UnwindToken { result_ty = unite_ty ~span ty_a ty_b; id = id_a }
    | V_UnwindToken _, _ -> fail ()
    | V_Target _, _ -> fail ()
    | V_ContextTy _, _ -> fail ()
    | V_Opaque _, _ -> fail ()
    | V_Blocked a, V_Blocked b -> V_Blocked (unite_blocked_value ~span a b)
    | V_Blocked _, _ -> fail ()
    | V_CompilerScope _, _ -> fail ()

  and unite_value : value Inference.unite =
   fun ~span { var = a; ty = ty_a } { var = b; ty = ty_b } ->
    {
      var = Inference.Var.unite unite_value_shape VarScopeImpl.unite ~span a b;
      ty = unite_ty ~span ty_a ty_b;
    }

  and unite_optional_name : optional_name Inference.unite =
   fun ~span { var = a } { var = b } ->
    {
      var =
        Inference.Var.unite
          (unite_option unite_name_shape)
          VarScopeImpl.unite ~span a b;
    }

  and unite_name : name Inference.unite =
   fun ~span { var = a } { var = b } ->
    { var = Inference.Var.unite unite_name_shape VarScopeImpl.unite ~span a b }

  and unite_option : 'a. 'a Inference.unite -> 'a option Inference.unite =
   fun unite_value ~span a b ->
    match (a, b) with
    | Some a, Some b -> Some (unite_value ~span a b)
    | Some value, None | None, Some value ->
        error span "Can't unite Some and None";
        Some value
    | None, None -> None

  and unite_name_instantiation : name_instantiation Inference.unite =
   fun ~span { generic = generic_a; arg = arg_a }
       { generic = generic_b; arg = arg_b } ->
    {
      generic = unite_value ~span generic_a generic_b;
      arg = unite_value ~span arg_a arg_b;
    }

  and unite_name_shape : name_shape Inference.unite =
   fun ~span a b ->
    let fail () =
      error span "name_shape %a != %a" print_name_shape a print_name_shape b;
      a
    in
    match (a, b) with
    | Simple a, Simple b -> Simple (unite_name_part ~span a b)
    | Simple a, _ -> fail ()
    | Concat (a1, a2), Concat (b1, b2) ->
        Concat (unite_name_shape ~span a1 b1, unite_name_part ~span a2 b2)
    | Concat _, _ -> fail ()
    | Instantiation a, Instantiation b ->
        Instantiation (unite_name_instantiation ~span a b)
    | Instantiation _, _ -> fail ()

  and unite_name_part : name_part Inference.unite =
   fun ~span a b ->
    let fail () =
      error span "name_part %a != %a" print_name_part a print_name_part b;
      a
    in
    match (a, b) with
    | Uri a, Uri b when Uri.equal a b -> Uri a
    | Uri _, _ -> fail ()
    | Str a, Str b when a = b -> Str a
    | Str _, _ -> fail ()
    | Symbol a, Symbol b when Symbol.equal a b -> Symbol a
    | Symbol _, _ -> fail ()

  and inferred_ty ~span shape : ty =
    { var = Inference.Var.new_inferred VarScope.of_ty_shape ~span shape }

  and inferred_value ~span shape : value =
    {
      var = Inference.Var.new_inferred VarScope.of_value_shape ~span shape;
      ty = ty_of_value_shape shape;
    }

  and infer_value_shape :
      scope:var_scope -> span:span -> ty_shape -> value_shape option =
   fun ~scope ~span ty_shape ->
    match ty_shape with
    | T_Unit -> Some V_Unit
    | T_Bool -> None
    | T_Int32 -> None
    | T_Int64 -> None
    | T_Float64 -> None
    | T_Char -> None
    | T_String -> None
    | T_Variant _ -> None
    | T_Ref _ -> None
    | T_Opaque _ -> None
    | T_Tuple ({ name = _; tuple } as ty) ->
        Some
          (V_Tuple
             {
               ty;
               tuple =
                 tuple
                 |> Tuple.map
                      (fun (field : ty_tuple_field) : value_tuple_field ->
                        {
                          place =
                            init_place ~mut:Inherit
                            <| new_not_inferred_value_of_ty ~scope ~span
                                 field.ty;
                          span;
                          ty_field = field;
                        });
             })
    | T_Ty -> Some (V_Ty (new_not_inferred_ty ~scope ~span))
    | T_Fn _ -> None
    | T_Generic _ -> None
    | T_Ast -> None
    | T_UnwindToken _ -> None
    | T_Target -> None
    | T_ContextTy -> None
    | T_CompilerScope -> None
    | T_Blocked _ -> None
    | T_Error -> None

  and new_not_inferred_ty ~scope ~span : ty =
    { var = Inference.Var.new_not_inferred ~scope ~span }

  and new_not_inferred_value ~scope ~span : value =
    let var = Inference.Var.new_not_inferred ~scope ~span in
    let ty = new_not_inferred_ty ~scope ~span in
    var
    |> Inference.Var.once_inferred (fun value_shape ->
        let _ : ty =
          unite_with_ctx unite_ty ~span (ty_of_value_shape value_shape) ty
        in
        ());
    ty.var
    |> Inference.Var.once_inferred (fun ty_shape ->
        with_ctx (fun () ->
            match infer_value_shape ~scope ~span ty_shape with
            | Some shape ->
                var
                |> Inference.Var.infer_as VarScope.of_value_shape
                     unite_value_shape VarScopeImpl.unite ~span shape
            | None -> ()));
    { var; ty }

  and new_not_inferred_value_of_ty ~scope ~span ty : value =
    let value = new_not_inferred_value ~scope ~span in
    let _ : ty = unite_ty ~span value.ty ty in
    value

  and ty_of_value_shape : value_shape -> ty =
    let span = Span.fake "<ty_of_shape>" in
    fun shape ->
      match shape with
      | V_Unit -> inferred_ty ~span T_Unit
      | V_Bool _ -> inferred_ty ~span T_Bool
      | V_Int32 _ -> inferred_ty ~span T_Int32
      | V_Int64 _ -> inferred_ty ~span T_Int64
      | V_Float64 _ -> inferred_ty ~span T_Float64
      | V_Char _ -> inferred_ty ~span T_Char
      | V_String _ -> inferred_ty ~span T_String
      | V_Opaque { ty; value = _ } -> inferred_ty ~span (T_Opaque ty)
      | V_Ref { mut; place } ->
          inferred_ty ~span
            (T_Ref
               {
                 mut =
                   {
                     var = Inference.Var.new_inferred VarScope.of_bool ~span mut;
                   };
                 referenced = place.ty;
               })
      | V_Tuple { ty; tuple = _ } -> inferred_ty ~span <| T_Tuple ty
      | V_Variant { ty; _ } -> inferred_ty ~span <| T_Variant ty
      | V_Ty _ -> inferred_ty ~span T_Ty
      | V_Fn { ty; _ } -> inferred_ty ~span <| T_Fn ty
      | V_Generic { id = _; name = _; fn = _; ty } ->
          inferred_ty ~span <| T_Generic ty
      | V_NativeFn { id = _; ty; name = _; impl = _ } ->
          inferred_ty ~span <| T_Fn ty
      | V_Ast _ -> inferred_ty ~span T_Ast
      | V_UnwindToken { result_ty; id = _ } ->
          inferred_ty ~span <| T_UnwindToken { result = result_ty }
      | V_Target _ -> inferred_ty ~span T_Target
      | V_ContextTy _ -> inferred_ty ~span T_ContextTy
      | V_CompilerScope _ -> inferred_ty ~span T_CompilerScope
      | V_Blocked b -> ty_of_blocked b
      | V_Error -> inferred_ty ~span T_Error

  and init_place ~mut value : place =
    { id = Id.gen (); state = Occupied value; ty = value.ty; mut }

  and ty_of_blocked : blocked_value -> ty = fun value -> value.ty
end

let unite_ty_shape = unite_with_ctx Impl.unite_ty_shape
let unite_blocked_value = unite_with_ctx Impl.unite_blocked_value
let unite_blocked_value_shape = unite_with_ctx Impl.unite_blocked_value_shape
let unite_ty_generic = unite_with_ctx Impl.unite_ty_generic
let unite_pattern ~span a b = with_ctx (fun () -> Impl.unite_pattern ~span a b)
let unite_ty_ref = unite_with_ctx Impl.unite_ty_ref
let unite_is_mutable = unite_with_ctx Impl.unite_is_mutable
let unite_bool = unite_with_ctx Impl.unite_bool
let unite_ty_tuple = unite_with_ctx Impl.unite_ty_tuple
let unite_ty_tuple_field = unite_with_ctx Impl.unite_ty_tuple_field
let unite_ty_variant_data = unite_with_ctx Impl.unite_ty_variant_data
let unite_ty_fn = unite_with_ctx Impl.unite_ty_fn
let unite_ty = unite_with_ctx Impl.unite_ty
let unite_place = unite_with_ctx Impl.unite_place
let unite_place_mut = unite_with_ctx Impl.unite_place_mut
let unite_value_shape = unite_with_ctx Impl.unite_value_shape
let unite_value = unite_with_ctx Impl.unite_value
let unite_optional_name = unite_with_ctx Impl.unite_optional_name
let unite_name = unite_with_ctx Impl.unite_name
let unite_name_instantiation = unite_with_ctx Impl.unite_name_instantiation
let unite_name_shape = unite_with_ctx Impl.unite_name_shape
let unite_name_part = unite_with_ctx Impl.unite_name_part
let inferred_ty ~span shape = with_ctx (fun () -> Impl.inferred_ty ~span shape)

let inferred_value ~span shape =
  with_ctx (fun () -> Impl.inferred_value ~span shape)

let infer_value_shape ~span shape =
  with_ctx (fun () -> Impl.infer_value_shape ~span shape)

let new_not_inferred_ty ~scope ~span =
  with_ctx (fun () -> Impl.new_not_inferred_ty ~scope ~span)

let unite_option unite_value = unite_with_ctx (Impl.unite_option unite_value)

let new_not_inferred_value ~scope ~span : value =
  with_ctx (fun () -> Impl.new_not_inferred_value ~scope ~span)

let new_not_inferred_value_of_ty ~scope ~span ty : value =
  with_ctx (fun () -> Impl.new_not_inferred_value_of_ty ~scope ~span ty)

let ty_of_value_shape = Impl.ty_of_value_shape
let init_place = Impl.init_place
let ty_of_blocked = Impl.ty_of_blocked
