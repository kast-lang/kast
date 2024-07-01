open Prelude

type value =
  | Ast of Ast.value
  | Macro of fn
  | BuiltinMacro of (state -> Ast.value StringMap.t -> evaled)
  | BuiltinFn of (value -> value)
  | Function of fn
  | Void
  | Bool of bool
  | Float of float
  | String of string
  | Dict of value StringMap.t

and fn = { captured : state; args_pattern : Ast.value; body : Ast.value }
and evaled = { value : value; new_bindings : value StringMap.t }
and state = { locals : value StringMap.t; syntax : Syntax.syntax }

let rec show = function
  | Ast ast -> "`(" ^ Ast.show ast ^ ")"
  | Void -> "void"
  | Macro _ -> "macro <...>"
  | BuiltinMacro _ -> "builtin_macro"
  | BuiltinFn _ -> "builtin"
  | Function _ -> "function <...>"
  | Float value -> Float.to_string value
  | Bool value -> Bool.to_string value
  | String value -> "\"" ^ String.escaped value ^ "\""
  | Dict values ->
      "{ "
      ^ StringMap.fold
          (fun name value acc ->
            (if acc = "" then "" else acc ^ ", ") ^ name ^ ": " ^ show value)
          values ""
      ^ " }"

let just_value value = { value; new_bindings = StringMap.empty }

let update_locals =
  StringMap.union (fun _name _prev new_value -> Some new_value)

let pattern_match_opt (pattern : Ast.value) (value : value) :
    value StringMap.t option =
  match pattern with
  | Nothing -> ( match value with Void -> Some StringMap.empty | _ -> None)
  | Simple spanned -> (
      match spanned.value with
      | Ident ident -> Some (StringMap.singleton ident value)
      | _ -> failwith "todo")
  | _ -> failwith "todo"

let pattern_match (pattern : Ast.value) (value : value) : value StringMap.t =
  match pattern_match_opt pattern value with
  | Some result -> result
  | None -> failwith "match failed"

let rec eval_ast (self : state) (ast : Ast.value) : evaled =
  match ast with
  | Nothing -> just_value Void
  | Simple { value = token; span = _ } ->
      just_value
        (match token with
        | Ident ident -> (
            match StringMap.find_opt ident self.locals with
            | None -> failwith (ident ^ " not found")
            | Some value -> value)
        | Number num -> Float (float_of_string num)
        | String { value; _ } -> String value
        | Punctuation _ -> failwith "punctuation")
  | Complex { def; values } -> eval_macro self def.name values
  | Syntax { def; value } -> eval_ast self value

and call_impl (f : fn) (args : value) : value =
  (eval_ast
     {
       f.captured with
       locals =
         update_locals f.captured.locals (pattern_match f.args_pattern args);
     }
     f.body)
    .value

and call (f : value) (args : value) : value =
  match f with
  | BuiltinFn f -> f args
  | Function f -> call_impl f args
  | _ -> failwith "not a function"

and eval_map (self : state) (values : Ast.value StringMap.t) : value =
  Dict (StringMap.map (fun ast -> (eval_ast self ast).value) values)

and eval_macro (self : state) (name : string) (values : Ast.value StringMap.t) :
    evaled =
  match StringMap.find_opt name self.locals with
  | None -> failwith (name ^ " not found")
  | Some value -> (
      match value with
      | BuiltinFn f -> just_value (f (eval_map self values))
      | BuiltinMacro f -> f self values
      | Macro f -> (
          let values = Dict (StringMap.map (fun x -> Ast x) values) in
          Log.trace ("call macro with " ^ show values);
          match call_impl f values with
          | Ast new_ast ->
              Log.trace ("macro expanded to " ^ Ast.show new_ast);
              eval_ast self new_ast
          | _ -> failwith "macro returned not an ast")
      | _ -> failwith (name ^ " is not a macro"))

let discard = function
  | Void -> ()
  | that -> failwith ("only void can be discarded (discarded " ^ show that ^ ")")

let get_field obj field =
  match obj with
  | Dict dict -> StringMap.find field dict
  | _ -> failwith "can't get field of this thang"

module Builtins = struct
  let call self args =
    let f = eval_ast self (StringMap.find "f" args) in
    let args = eval_ast self (StringMap.find "args" args) in
    just_value (call f.value args.value)

  let then' self args =
    let { value = a; new_bindings = a_new_bindings } =
      eval_ast self (StringMap.find "a" args)
    in
    discard a;
    let self_with_new_bindings =
      { self with locals = update_locals self.locals a_new_bindings }
    in
    match StringMap.find_opt "b" args with
    | Some b ->
        let result = eval_ast self_with_new_bindings b in
        {
          result with
          new_bindings = update_locals a_new_bindings result.new_bindings;
        }
    | None -> { value = Void; new_bindings = a_new_bindings }

  let print (args : value) : value =
    match args with
    | String value ->
        print_endline value;
        Void
    | _ -> failwith "print expected a string"

  let if' self args =
    let cond = eval_ast self (StringMap.find "cond" args) in
    let self_with_new_bindings =
      { self with locals = update_locals self.locals cond.new_bindings }
    in
    let cond =
      match cond.value with
      | Bool value -> value
      | _ -> failwith "condition must be a bool"
    in
    let then' = StringMap.find "then" args in
    let else' = StringMap.find_opt "else" args in
    match else' with
    | Some else' ->
        if cond then eval_ast self_with_new_bindings then'
        else eval_ast self_with_new_bindings else'
    | None ->
        (if cond then
           let value = (eval_ast self_with_new_bindings then').value in
           discard value);
        just_value Void

  let dict_fn f = function Dict args -> f args | _ -> failwith "expected dict"

  let binary_op_with lhs rhs f =
    dict_fn (fun args ->
        let lhs = StringMap.find lhs args in
        let rhs = StringMap.find rhs args in
        match (lhs, rhs) with
        | Float lhs, Float rhs -> Float (f lhs rhs)
        | _ -> failwith "only floats")

  let binary_op = binary_op_with "lhs" "rhs"

  let float_fn f = function
    | Float value -> Float (f value)
    | _ -> failwith "only floats"

  let single_arg_fn name f =
    dict_fn (fun args ->
        let value = StringMap.find name args in
        f value)

  let float_macro name f =
    let f = float_fn f in
    single_arg_fn name f

  let unary_op = float_macro "x"

  let quote self args =
    let rec impl : Ast.value -> Ast.value = function
      | Complex { def = { name = "unquote"; _ }; values; _ } -> (
          let inner = StringMap.find "expr" values in
          Log.trace ("unquoting" ^ Ast.show inner);
          match (eval_ast self inner).value with
          | Ast inner -> inner
          | other ->
              failwith
                ("unquoted things should be asts (was " ^ show other ^ ")"))
      | Nothing -> Nothing
      | Simple token -> Simple token
      | Complex { def; values } ->
          Complex { def; values = StringMap.map impl values }
      | Syntax { def; value } -> Syntax { def; value = impl value }
    in
    let expr = StringMap.find "expr" args in
    Log.trace ("quoting " ^ Ast.show expr);
    just_value (Ast (impl expr))

  let let' self args =
    let pattern = StringMap.find "pattern" args in
    let value = StringMap.find "value" args in
    let value = (eval_ast self value).value in
    { value = Void; new_bindings = pattern_match pattern value }

  let function_def self (args : Ast.value StringMap.t) =
    let args_pattern =
      match StringMap.find_opt "args" args with
      | Some pat -> pat
      | None -> Nothing
    in
    let body = StringMap.find "body" args in
    just_value (Function { captured = self; args_pattern; body })

  let field_access self args =
    let obj = StringMap.find "obj" args in
    let obj = (eval_ast self obj).value in
    let field = StringMap.find "field" args in
    match field with
    | Simple { value = Ident ident; _ } -> just_value (get_field obj ident)
    | _ -> failwith "field access must be using an ident"

  let macro = function
    | Function f -> Macro f
    | _ -> failwith "expected a function"

  let all =
    StringMap.of_list
      [
        ("print", BuiltinFn print);
        ("call", BuiltinMacro call);
        ("then", BuiltinMacro then');
        ("if", BuiltinMacro if');
        ("uplus", BuiltinFn (unary_op ( ~+. )));
        ("negate", BuiltinFn (unary_op ( ~-. )));
        ("add", BuiltinFn (binary_op ( +. )));
        ("sub", BuiltinFn (binary_op ( -. )));
        ("mul", BuiltinFn (binary_op ( *. )));
        ("div", BuiltinFn (binary_op ( /. )));
        (* ("mod", BuiltinFn (binary_op ( Stdlib.rem ))); *)
        ("pow", BuiltinFn (binary_op ( ** )));
        ("sin", BuiltinFn (float_fn sin));
        ("cos", BuiltinFn (float_fn cos));
        ("sqrt", BuiltinFn (float_fn sqrt));
        ("quote", BuiltinMacro quote);
        ("scope", BuiltinFn (single_arg_fn "e" (fun x -> x)));
        ("unit", BuiltinFn (fun _ -> Void));
        ("let", BuiltinMacro let');
        ("function_def", BuiltinMacro function_def);
        ("field_access", BuiltinMacro field_access);
        ("macro", BuiltinFn macro);
      ]
end

let empty () : state = { locals = Builtins.all; syntax = Syntax.empty }

let eval (self : state ref) (s : string) ~(filename : string) : value =
  let tokens = Lexer.parse s (Filename filename) in
  let ast = Ast.parse !self.syntax tokens in
  let result = eval_ast !self ast in
  let rec extend_syntax syntax = function
    | Ast.Syntax { def; value } ->
        extend_syntax (Syntax.add_syntax def syntax) value
    | _ -> syntax
  in
  self :=
    {
      syntax = extend_syntax !self.syntax ast;
      locals = update_locals !self.locals result.new_bindings;
    };
  result.value

let eval_file (self : state ref) (filename : string) : value =
  let f = open_in filename in
  let contents = really_input_string f (in_channel_length f) in
  close_in f;
  let value = eval self contents ~filename in
  Log.trace ("after " ^ filename ^ " syntax:");
  Log.trace (Syntax.show !self.syntax);
  value
