open Prelude

type value =
  | Todo of Ast.value
  | Macro
  | BuiltinMacro of (state -> Ast.value StringMap.t -> value)
  | BuiltinFn of (value -> value)
  | Void
  | Bool of bool
  | Float of float
  | String of string
  | Dict of value StringMap.t

and state = { locals : value StringMap.t; syntax : Syntax.syntax }

let rec show = function
  | Todo ast -> Ast.show ast
  | Void -> "void"
  | Macro -> "macro <...>"
  | BuiltinMacro _ -> "builtin_macro"
  | BuiltinFn _ -> "builtin"
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

let rec eval_ast (self : state) (ast : Ast.value) : value =
  match ast with
  | Nothing -> Void
  | Simple { value = token; span = _ } -> (
      match token with
      | Ident ident -> (
          match StringMap.find_opt ident self.locals with
          | None -> failwith (ident ^ " not found")
          | Some value -> value)
      | Number num -> Float (float_of_string num)
      | String { value; _ } -> String value
      | Punctuation _ -> failwith "punctuation")
  | Complex { def; values } -> eval_macro self def.name values
  | Syntax { def; value } -> eval_ast self value

and call (self : state) (f : value) (args : value) : value =
  match f with BuiltinFn f -> f args | _ -> failwith "not a function"

and eval_map (self : state) (values : Ast.value StringMap.t) : value =
  Dict (StringMap.map (eval_ast self) values)

and eval_macro (self : state) (name : string) (values : Ast.value StringMap.t) :
    value =
  match StringMap.find_opt name self.locals with
  | None -> failwith (name ^ " not found")
  | Some value -> (
      match value with
      | BuiltinFn f -> f (eval_map self values)
      | BuiltinMacro f -> f self values
      | Macro -> failwith "todo"
      | _ -> failwith (name ^ " is not a macro"))

let discard = function Void -> () | _ -> failwith "only void can be discarded"

module Builtins = struct
  let call (self : state) (args : Ast.value StringMap.t) : value =
    let f = eval_ast self (StringMap.find "f" args) in
    let args = eval_ast self (StringMap.find "args" args) in
    call self f args

  let then' self args =
    let a = eval_ast self (StringMap.find "a" args) in
    discard a;
    match StringMap.find_opt "b" args with
    | Some b -> eval_ast self b
    | None -> Void

  let print (args : value) : value =
    match args with
    | String value ->
        print_endline value;
        Void
    | _ -> failwith "print expected a string"

  let if' self args =
    let cond =
      match eval_ast self (StringMap.find "cond" args) with
      | Bool value -> value
      | _ -> failwith "condition must be a bool"
    in
    let then' = StringMap.find "then" args in
    let else' = StringMap.find_opt "else" args in
    match else' with
    | Some else' -> if cond then eval_ast self then' else eval_ast self else'
    | None ->
        (if cond then
           let value = eval_ast self then' in
           discard value);
        Void

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
        ("parens", BuiltinFn (single_arg_fn "e" (fun x -> x)));
        ("unit", BuiltinFn (fun _ -> Void));
      ]
end

let empty () : state = { locals = Builtins.all; syntax = Syntax.empty }

let eval (self : state ref) (s : string) ~(filename : string) : value =
  let tokens = Lexer.parse s (Filename filename) in
  let ast = Ast.parse !self.syntax tokens in
  let value = eval_ast !self ast in
  let rec extend_syntax syntax = function
    | Ast.Syntax { def; value } ->
        extend_syntax (Syntax.add_syntax def syntax) value
    | _ -> syntax
  in
  self := { !self with syntax = extend_syntax !self.syntax ast };
  value

let eval_file (self : state ref) (filename : string) : value =
  let f = open_in filename in
  let contents = really_input_string f (in_channel_length f) in
  close_in f;
  eval self contents ~filename
