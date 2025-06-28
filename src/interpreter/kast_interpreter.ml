open Std
open Kast_util
open Kast_types

type bindings = value StringMap.t
type scope = { bindings : bindings }
type state = { scope : scope }

let builtins =
  let native_fn name impl : string * value =
    (name, { shape = V_NativeFn { name; impl } })
  in
  [
    native_fn "print" (fun value ->
        (match value.shape with
        | V_String s -> println "%s" s
        | _ -> fail "print expected a string");
        { shape = V_Unit });
  ]

let init : unit -> state =
 fun () -> { scope = { bindings = StringMap.of_list builtins } }

let pattern_match : value -> pattern -> bindings =
 fun value pattern ->
  match pattern.shape with
  | P_Placeholder -> StringMap.empty
  | P_Binding binding -> StringMap.singleton binding.name value

let rec eval : state -> expr -> value =
 fun state expr ->
  match expr.shape with
  | E_Constant value -> value
  | E_Binding binding -> StringMap.find binding.name state.scope.bindings
  | E_Fn fn -> { shape = V_Fn fn }
  | E_Apply { f; arg } -> (
      let f = eval state f in
      let arg = eval state arg in
      match f.shape with
      | V_Fn f ->
          let new_bindings = pattern_match arg f.arg in
          let new_state =
            {
              scope =
                {
                  bindings =
                    StringMap.union
                      (fun _name _old_value new_value -> Some new_value)
                      state.scope.bindings new_bindings;
                };
            }
          in
          let result = eval new_state f.body in
          result
      | V_NativeFn f -> f.impl arg
      | _ -> fail "expected fn")
