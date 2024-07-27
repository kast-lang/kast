open Prelude
open Types

exception FailedUnite of string

type var = inference_var

module Make
    (Show : Modules.Show)
    (Compiler : Modules.Compiler)
    (Interpreter : Modules.Interpreter) : Modules.Inference = struct
  open Show
  open Compiler
  open Interpreter

  let new_var () =
    {
      data = Root { inferred = None; type_var = None; checks = [] };
      id = Id.gen ();
    }

  let rec get_root_var : var -> var =
   fun var ->
    match var.data with
    | Root _ -> var
    | SameAs closer_to_root ->
        let root = get_root_var closer_to_root in
        var.data <- SameAs root;
        root

  and show_id var = Id.show (get_root_var var).id
  and get_id var = (get_root_var var).id

  and get_root_data var =
    match var.data with
    | SameAs _ -> failwith "expected a root"
    | Root data -> data

  and get_inferred var =
    let root = get_root_var var in
    let data = get_root_data root in
    data.inferred

  and get_type var =
    let root = get_root_var var in
    let data = get_root_data root in
    InferVar
      (match data.type_var with
      | Some t -> t
      | None ->
          let t = new_set_var (Type (InferVar (new_var ())) : value) in
          data.type_var <- Some t;
          t)

  and check_again (data : inference_data) : unit =
    match data.inferred with
    | Some value ->
        data.checks
        |> List.iter (fun f -> if not (f value) then failwith "check failed")
    | None -> ()

  and make_same a b =
    let a = get_root_var a in
    let b = get_root_var b in
    if a != b then
      let a_data = get_root_data a in
      let b_data = get_root_data b in
      let inferred_value =
        match (a_data.inferred, b_data.inferred) with
        | Some inferred, None | None, Some inferred -> Some inferred
        | None, None -> None
        | Some inferred_a, Some inferred_b -> Some (unite inferred_a inferred_b)
      in
      if Random.bool () then (
        a_data.inferred <- inferred_value;
        b.data <- SameAs a)
      else (
        b_data.inferred <- inferred_value;
        a.data <- SameAs b)

  and set var value =
    let root = get_root_var var in
    let data = get_root_data root in
    match data.inferred with
    | None -> data.inferred <- Some value
    | Some current_value -> data.inferred <- Some (unite current_value value)

  and add_check var f =
    let root = get_root_var var in
    let data = get_root_data root in
    data.checks <- f :: data.checks;
    check_again data

  and new_set_var value =
    let var = new_var () in
    set var value;
    var

  and unite_contexts (a : contexts_type) (b : contexts_type) : contexts_type = a
  (* TODO if a = b then a else raise @@ Inference.FailedUnite "contexts dont match" *)

  and failinfer : 'a. unit -> 'a =
   fun () -> raise @@ FailedUnite "inference union failed"

  and unite_types (a : value_type) (b : value_type) : value_type =
    try
      match (a, b) with
      (* InferVars patterns must be at the top here *)
      | InferVar a, InferVar b ->
          make_same a b;
          InferVar a
      | InferVar var, b ->
          set var (Type b : value);
          (* TODO here something can be united? should result in that union? *)
          b
      | a, InferVar var ->
          set var (Type a : value);
          (* TODO here something can be united? should result in that union? *)
          a
      | Binding a, Binding b when a.id = b.id -> Binding a
      | Binding _, _ -> failinfer ()
      | Void, Void -> Void
      | Void, _ -> failinfer ()
      | UnwindToken, UnwindToken -> UnwindToken
      | UnwindToken, _ -> failinfer ()
      | DelimitedToken, DelimitedToken -> DelimitedToken
      | DelimitedToken, _ -> failinfer ()
      | Never, Never -> Never
      | Never, _ -> failinfer ()
      | Ast, Ast -> Ast
      | Ast, _ -> failinfer ()
      | Bool, Bool -> Bool
      | Bool, _ -> failinfer ()
      | Int32, Int32 -> Int32
      | Int32, _ -> failinfer ()
      | Int64, Int64 -> Int64
      | Int64, _ -> failinfer ()
      | Float32, Float32 -> Float32
      | Float32, _ -> failinfer ()
      | Float64, Float64 -> Float64
      | Float64, _ -> failinfer ()
      | String, String -> String
      | String, _ -> failinfer ()
      | BuiltinMacro, _ -> failwith "inferred builtin macro type?"
      | Type, Type -> Type
      | Type, _ -> failinfer ()
      | Fn a, Fn b ->
          Fn
            {
              arg_type = unite_types a.arg_type b.arg_type;
              result_type = unite_types a.result_type b.result_type;
              contexts = unite_contexts a.contexts b.contexts;
            }
      | Fn _, _ -> failinfer ()
      | Macro a, Macro b ->
          Macro
            {
              arg_type = unite_types a.arg_type b.arg_type;
              result_type = unite_types a.result_type b.result_type;
              contexts = unite_contexts a.contexts b.contexts;
            }
      | Macro _, _ -> failinfer ()
      | Template a, Template b ->
          (if a != b then
             let compiled_a = ensure_compiled a in
             let compiled_b = ensure_compiled b in
             let bindings_a = pattern_bindings compiled_a.args in
             let bindings_b = pattern_bindings compiled_b.args in
             let vars_args : value =
               (* TODO pattern -> value with vars in place of bindings *)
               Dict
                 {
                   fields =
                     StringMap.match_map
                       (fun _name a b : value ->
                         let var = new_var () in
                         make_same var a.value_type;
                         make_same var b.value_type;
                         Var { id = Id.gen (); typ = InferVar var })
                       bindings_a bindings_b;
                 }
             in
             let var_inst_a =
               call_compiled empty_contexts (ensure_compiled a) vars_args
             in
             let var_inst_b =
               call_compiled empty_contexts (ensure_compiled b) vars_args
             in
             ignore @@ unite var_inst_a var_inst_b);
          Template a
      | Template _, _ -> failinfer ()
      | NewType _, _ -> failwith "todo newtype was inferred"
      | Dict a, Dict b ->
          Dict
            {
              fields =
                StringMap.match_map (fun _name -> unite_types) a.fields b.fields;
            }
      | Dict _, _ -> failinfer ()
      | OneOf a, OneOf b ->
          OneOf
            (StringMap.match_map
               (fun _name a b ->
                 match (a, b) with
                 | None, None -> None
                 | Some a, Some b -> Some (unite_types a b)
                 | None, Some _ | Some _, None -> failinfer ())
               a b)
      | OneOf _, _ -> failinfer ()
      | Union _, _ -> failwith "todo union"
      | Var a, Var b -> if a.id = b.id then Var a else failinfer ()
      | Var _, _ -> failinfer ()
      | MultiSet _, _ -> failwith "todo inferred multiset"
    with FailedUnite _ as failure ->
      Log.error @@ "  while uniting " ^ show_type a ^ " and " ^ show_type b;
      raise failure

  and unite (a : value) (b : value) : value =
    try
      match (a, b) with
      | Binding a, Binding b when a.id = b.id -> Binding a
      | Binding _, _ -> failinfer ()
      | InferVar a, InferVar b ->
          make_same a b;
          InferVar a
      | InferVar a, b ->
          set a b;
          (* TODO here something can be united? should result in that union? *)
          b
      | a, InferVar b ->
          set b a;
          (* TODO here something can be united? should result in that union? *)
          a
      | Var a, Var b when a.id = b.id -> Var a
      | Var _, _ -> failinfer ()
      | Void, Void -> Void
      | Void, _ -> failinfer ()
      | UnwindToken a, UnwindToken b ->
          if a = b then UnwindToken a else failinfer ()
      | UnwindToken _, _ -> failinfer ()
      | DelimitedToken a, DelimitedToken b ->
          if a = b then DelimitedToken a else failinfer ()
      | DelimitedToken _, _ -> failinfer ()
      | Ast a, Ast b -> if a = b then Ast a else failinfer ()
      | Ast _, _ -> failinfer ()
      | Macro _, _ -> failwith "inferred macro?"
      | Function _, _ -> failwith "inferred function?"
      | Template _, _ -> failwith "inferred template?"
      | BuiltinMacro _, _ -> failwith "inferred builtinmacro?"
      | BuiltinFn _, _ -> failwith "inferred builtinfn?"
      | Bool a, Bool b -> if a = b then Bool a else failinfer ()
      | Bool _, _ -> failinfer ()
      | Int32 a, Int32 b -> if a = b then Int32 a else failinfer ()
      | Int32 _, _ -> failinfer ()
      | Int64 a, Int64 b -> if a = b then Int64 a else failinfer ()
      | Int64 _, _ -> failinfer ()
      | Float64 a, Float64 b -> if a = b then Float64 a else failinfer ()
      | Float64 _, _ -> failinfer ()
      | String a, String b -> if a = b then String a else failinfer ()
      | String _, _ -> failinfer ()
      | Dict { fields = a }, Dict { fields = b } ->
          Dict { fields = StringMap.match_map (fun _name a b -> unite a b) a b }
      | Dict _, _ -> failinfer ()
      | Struct _, _ -> failwith "inferred struct?"
      | Variant _, _ -> failwith "inferred variant?"
      | Ref _, _ -> failwith "inferred ref?"
      | Type a, Type b -> Type (unite_types a b)
      | Type _, _ -> failinfer ()
    with FailedUnite _ as failure ->
      Log.error @@ "  while uniting values " ^ show a ^ " and " ^ show b;
      raise failure

  and type_check_contexts (expected : contexts_type) (actual : contexts_type)
      (variables : type_var_map ref) : bool =
    Seq.for_all
      (fun ((expected_type_id, expected_amount) : id * int) ->
        let actual_amount =
          match Id.Map.find_opt expected_type_id expected with
          | Some amount -> amount
          | None -> 0
        in
        actual_amount <= expected_amount)
      (Id.Map.to_seq actual)
end
