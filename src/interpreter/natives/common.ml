include Std
include Kast_util
include Kast_types
include Kast_interpreter_core
module Inference = Kast_inference

let span = Span.of_ocaml __POS__

let single_arg ~span (args : value) : value =
  let args = args |> Value.expect_tuple |> Option.unwrap in
  let arg = args.tuple |> Tuple.unwrap_single_unnamed in
  claim ~span arg.place
;;

let make_args ~span (args : value tuple) (ty : Types.ty_tuple) : value =
  V_Tuple
    { tuple =
        Tuple.zip_order_a args ty.tuple
        |> Tuple.map (fun (arg, ty_field) : Types.value_tuple_field ->
          { place = Place.init ~mut:Inherit arg; span; ty_field })
    ; ty
    }
  |> Value.inferred ~span
;;

let make_single_arg ~span (arg : value) (ty : Types.ty_tuple) : value =
  make_args ~span (Tuple.make [ arg ] []) ty
;;

let make_single_arg_infer ~span (arg : value) : value =
  let field_ty : Types.ty_tuple_field =
    { ty = Value.ty_of arg; symbol = None; label = None }
  in
  make_args
    ~span
    (Tuple.make [ arg ] [])
    ({ name = OptionalName.new_not_inferred ~span ~scope:(VarScope.root ())
     ; tuple = Tuple.make [ field_ty ] []
     }
     : Types.ty_tuple)
;;

let native_fn name impl : string * (ty -> value) =
  ( name
  , fun ty ->
      let scope = VarScope.of_ty ty in
      let fn_ty : Types.ty_fn =
        { is_closure = Inference.simple ~span (true : bool)
        ; call_convention = Inference.simple ~span (None : string option)
        ; args = { ty = Ty.new_not_inferred ~scope ~span }
        ; result = Ty.new_not_inferred ~scope ~span
        }
      in
      ty |> Inference.Ty.expect_inferred_as ~span (T_Fn fn_ty |> Ty.inferred ~span);
      V_NativeFn { id = Id.gen (); ty = fn_ty; name; impl = impl fn_ty }
      |> Value.inferred ~span )
;;

let construct_tuple ~span (ty : Types.ty_tuple) (tuple : (ty -> value) tuple) : value =
  with_return (fun ({ return } : Value.shape return_handle) : Value.shape ->
    let zipped =
      try Tuple.zip_order_a ty.tuple tuple with
      | Invalid_argument s ->
        Error.error span "%S" s;
        return V_Error
    in
    V_Tuple
      { ty
      ; tuple =
          zipped
          |> Tuple.map
               (fun
                   ((ty_field, value) : Types.ty_tuple_field * (ty -> value))
                    : Types.value_tuple_field
                  ->
                  let value = value ty_field.ty in
                  { place = Place.init ~mut:Inherit value; span; ty_field })
      })
  |> Value.inferred ~span
;;

let construct_variant
      ~span
      (ty : Types.ty_variant)
      (variant : string)
      (data : (ty -> value) option)
  : value
  =
  with_return (fun ({ return } : Value.shape return_handle) : Value.shape ->
    match ty.variants |> Row.await_find_opt variant with
    | None ->
      Error.error span "Did not find variant %a" String.print_debug variant;
      V_Error
    | Some (label, data_ty) ->
      let data =
        match data, data_ty.data with
        | None, None -> None
        | None, Some _ ->
          Error.error span "Variant expected data, got no data";
          return V_Error
        | Some _, None ->
          Error.error span "Variant expected no data, got some data";
          return V_Error
        | Some data, Some data_ty ->
          let data = data data_ty in
          Value.ty_of data |> Inference.Ty.expect_inferred_as ~span data_ty;
          Some data
      in
      V_Variant { label; data = data |> Option.map (Place.init ~mut:Inherit); ty })
  |> Value.inferred ~span
;;

let construct_list ~span (ty : Types.ty_list) (values : value list) : value =
  V_List
    { ty; elements = values |> List.map (Place.init ~mut:Inherit) |> Dynarray.of_list }
  |> Value.inferred ~span
;;

let construct_slist ~span list_ty (f : ty -> value list) : value =
  let list_ty =
    list_ty |> Ty.await_inferred |> Ty.Shape.expect_variant |> Option.unwrap
  in
  let list_name = list_ty.name |> OptionalName.await_inferred |> Option.unwrap in
  let elem_ty =
    match list_name with
    | Instantiation { generic = _; arg } ->
      let arg = single_arg ~span arg in
      arg
      |> Value.expect_ty
      |> Option.unwrap_or_else (fun () ->
        fail "List instantiated not with type but with %a" Value.print arg)
    | _ -> fail "impossible :)"
  in
  let rec construct = function
    | [] -> construct_variant ~span list_ty "Nil" None
    | value :: tail ->
      construct_variant
        ~span
        list_ty
        "Cons"
        (Some
           (fun data_ty ->
             let data_ty =
               data_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.unwrap
             in
             construct_tuple
               ~span
               data_ty
               (Tuple.make
                  []
                  [ ("value", fun _ -> value); ("tail", fun _ -> construct tail) ])))
  in
  construct (f elem_ty)
;;
