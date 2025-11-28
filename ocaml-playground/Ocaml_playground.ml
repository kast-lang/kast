type id = int

let new_id () : id = 0

type ty =
  | Int32
  | Fn of {
      arg : ty;
      result : ty;
    }
  | Binding of binding
  | Var of var

and binding = { name : string }
and var = { id : id }

let once_inferred : var -> (ty -> unit) -> unit =
 fun _var _f -> failwith "some impl"

let unite : ty -> ty -> unit = fun _a _b -> failwith "some impl"

(* let id = [T] x :: T => x *)
(* id[int32] :: int32 -> int32 *)
(* we need to substitute
      T -> T with T = int32
      int32 -> int32
 *)

module StringMap = Map.Make (String)

let rec sub : ty StringMap.t -> ty -> ty =
 fun bindings ty ->
  match ty with
  | Int32 -> Int32
  | Fn { arg; result } ->
      Fn { arg = sub bindings arg; result = sub bindings result }
  | Binding { name } -> (
      match StringMap.find_opt name bindings with
      | Some substitution -> substitution
      | None -> Binding { name })
  | Var var ->
      let result = Var { id = new_id () } in
      once_inferred var (fun var_ty ->
          let subbed_var = sub bindings var_ty in
          unite result subbed_var);
      result

let () =
  let subbed_id =
    sub
      (StringMap.singleton "T" Int32)
      (Fn { arg = Binding { name = "T" }; result = Binding { name = "T" } })
  in
  assert (subbed_id = Fn { arg = Int32; result = Int32 })
