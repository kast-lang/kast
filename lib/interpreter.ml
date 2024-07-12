open Prelude

type ast_data = { span : Span.span; mutable ir : Ir.t option }
type ast = ast_data Ast.node

type value =
  | Ast of ast
  | Macro of fn
  | BuiltinMacro of builtin_macro
  | BuiltinFn of (value -> value)
  | Function of fn
  | Void
  | Bool of bool
  | Float of float
  | String of string
  | Dict of value StringMap.t
  | Struct of struct'
  | Ref of value ref

and builtin_macro = state -> ast StringMap.t -> compiled

and ir =
  | Void
  | Dict of ir StringMap.t
  | Ast of { def : Syntax.syntax_def; data : ast_data; values : ir StringMap.t }
  | FieldAccess of { obj : ir; name : string }
  | Const of value
  | Binding of binding
  | Number of string
  | String of { raw : string; value : string }
  | Discard of ir
  | Then of ir * ir
  | Call of { f : ir; args : ir }
  | BuiltinFn of (value -> value)
  | If of { cond : ir; then_case : ir; else_case : ir }
  | Let of { pattern : pattern; value : ir }

and fn = {
  captured : state;
  args_pattern : ast option;
  body : ast;
  mutable compiled : compiled_fn option;
}

and compiled_fn = { captured : state; args : pattern; body : ir }
and pattern = Void | Binding of binding
and evaled = { value : value; new_bindings : value StringMap.t }
and compiled = { ir : ir; new_bindings : state_local StringMap.t }
and struct' = { parent : struct' option; mutable data : state_data }
and state = { self : struct'; data : state_data }
and state_data = { locals : state_local StringMap.t; syntax : Syntax.syntax }
and state_local = Value of value | Binding of binding
and binding = { name : string }

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
  | Ref value -> show !value
  | Struct _ -> "struct <...>"

let just_value value = { value; new_bindings = StringMap.empty }

let update_locals =
  StringMap.union (fun _name _prev new_value -> Some new_value)

let pattern_match_opt (pattern : pattern) (value : value) :
    value StringMap.t option =
  match pattern with
  | Void -> ( match value with Void -> Some StringMap.empty | _ -> None)
  | Binding { name; _ } -> Some (StringMap.singleton name value)

let pattern_match (pattern : pattern) (value : value) : value StringMap.t =
  match pattern_match_opt pattern value with
  | Some result -> result
  | None -> failwith "match failed"

let get_local_opt (self : state) (name : string) : state_local option =
  match StringMap.find_opt name self.data.locals with
  | Some local -> Some local
  | None ->
      let rec find_in_scopes s =
        match StringMap.find_opt name s.data.locals with
        | Some local -> Some local
        | None -> (
            match s.parent with
            | Some parent -> find_in_scopes parent
            | None -> None)
      in
      find_in_scopes self.self

let get_local_value_opt (self : state) (name : string) : value option =
  Option.map
    (function
      | Value (Ref value) -> !value
      | Value other -> other
      | Binding _ -> failwith (name ^ " is a runtime value"))
    (get_local_opt self name)

let rec compile_ast (self : state) (ast : ast) : compiled =
  match ast with
  | Nothing _ -> { ir = Void; new_bindings = StringMap.empty }
  | Simple { token; _ } ->
      {
        ir =
          (match token with
          | Ident ident -> (
              match get_local_opt self ident with
              | None -> failwith (ident ^ " not found")
              | Some (Value value) -> Const value
              | Some (Binding binding) -> Binding binding)
          | Number num -> Number num
          | String { value; raw } -> String { value; raw }
          | Punctuation _ -> failwith "punctuation");
        new_bindings = StringMap.empty;
      }
  | Complex { def; values; _ } -> expand_macro self def.name values
  | Syntax { def; value; _ } -> compile_ast self value

and compile_pattern (self : state) (pattern : ast option) : pattern =
  match pattern with
  | None -> Void
  | Some ast -> (
      match ast with
      | Nothing _ -> Void
      | Simple { token; _ } -> (
          match token with
          | Ident name -> Binding { name }
          | _ -> failwith "expected an identifier")
      | _ -> failwith "todo ")

and pattern_bindings (pattern : pattern) : binding StringMap.t =
  match pattern with
  | Void -> StringMap.empty
  | Binding binding -> StringMap.singleton binding.name binding

and expand_macro (self : state) (name : string) (values : ast StringMap.t) :
    compiled =
  match get_local_opt self name with
  | None -> failwith (name ^ " not found")
  | Some (Binding _binding) ->
      failwith (name ^ " is a runtime value, not a macro")
  | Some (Value value) -> (
      match value with
      | BuiltinFn f ->
          let args : ir =
            Dict (StringMap.map (fun arg -> (compile_ast self arg).ir) values)
          in
          {
            ir = Call { f = BuiltinFn f; args };
            new_bindings = StringMap.empty;
          }
      | BuiltinMacro f -> f self values
      | Macro f -> (
          let values = Dict (StringMap.map (fun x -> Ast x) values) in
          Log.trace ("call macro with " ^ show values);
          match compile_and_call f values with
          | Ast new_ast ->
              Log.trace ("macro expanded to " ^ Ast.show new_ast);
              compile_ast self new_ast
          | _ -> failwith "macro returned not an ast")
      | _ -> failwith (name ^ " is not a macro"))

and eval_ast (self : state) (ast : ast) : evaled =
  let compiled = compile_ast self ast in
  eval_ir self compiled.ir

and eval_ir (self : state) (ir : ir) : evaled =
  match ir with
  | Void -> just_value Void
  | BuiltinFn f -> just_value (BuiltinFn f)
  | Dict values ->
      just_value
        (Dict (StringMap.map (fun value -> (eval_ir self value).value) values))
  | Ast { def; data; values } ->
      just_value
        (Ast
           (Complex
              {
                def;
                data;
                values =
                  StringMap.map
                    (fun value ->
                      match (eval_ir self value).value with
                      | Ast ast -> ast
                      | _ -> failwith "expected an ast")
                    values;
              }))
  | FieldAccess { obj; name } ->
      let obj = (eval_ir self obj).value in
      just_value (get_field obj name)
  | Const value -> just_value value
  | Binding binding -> (
      match get_local_value_opt self binding.name with
      | None -> failwith (binding.name ^ " not found wtf, we are compiled")
      | Some value -> just_value value)
  | Number s -> just_value (Float (Float.of_string s))
  | String { value; _ } -> just_value (String value)
  | Discard ir ->
      discard (eval_ir self ir).value;
      just_value Void
  | Then (a, b) ->
      let a = eval_ir self a in
      discard a.value;
      eval_ir
        {
          self with
          data =
            {
              self.data with
              locals =
                update_locals self.data.locals
                  (StringMap.map (fun value -> Value value) a.new_bindings);
            };
        }
        b
  | Call { f; args } ->
      let f = (eval_ir self f).value in
      let args = (eval_ir self args).value in
      just_value (call f args)
  | If { cond; then_case; else_case } -> (
      let cond = eval_ir self cond in
      let self_with_new_bindings =
        {
          self with
          data =
            {
              self.data with
              locals =
                update_locals self.data.locals
                  (StringMap.map (fun value -> Value value) cond.new_bindings);
            };
        }
      in
      match cond.value with
      | Bool true -> eval_ir self_with_new_bindings then_case
      | Bool false -> eval_ir self_with_new_bindings else_case
      | _ -> failwith "condition must be a bool")
  | Let { pattern; value } ->
      {
        value = Void;
        new_bindings = pattern_match pattern (eval_ir self value).value;
      }

and compile_and_call (f : fn) (args : value) : value =
  call_compiled (ensure_compiled f) args

and ensure_compiled (f : fn) : compiled_fn =
  if Option.is_none f.compiled then
    f.compiled <-
      (let args = compile_pattern f.captured f.args_pattern in
       Some
         {
           captured = f.captured;
           args;
           body =
             (compile_ast
                {
                  f.captured with
                  data =
                    {
                      f.captured.data with
                      locals =
                        update_locals f.captured.data.locals
                          (StringMap.map
                             (fun binding : state_local -> Binding binding)
                             (pattern_bindings args));
                    };
                }
                f.body)
               .ir;
         });
  Option.get f.compiled

and call_compiled (f : compiled_fn) (args : value) : value =
  (eval_ir
     {
       f.captured with
       data =
         {
           f.captured.data with
           locals =
             update_locals f.captured.data.locals
               (StringMap.mapi
                  (fun name value ->
                    Log.trace ("called with arg " ^ name ^ " = " ^ show value);
                    Value value)
                  (pattern_match f.args args));
         };
     }
     f.body)
    .value

and call (f : value) (args : value) : value =
  match f with
  | BuiltinFn f -> f args
  | Function f -> compile_and_call f args
  | _ -> failwith "not a function"

and eval_map (self : state) (values : ast StringMap.t) : value =
  Dict (StringMap.map (fun ast -> (eval_ast self ast).value) values)

and discard : value -> unit = function
  | Void -> ()
  | that -> failwith ("only void can be discarded (discarded " ^ show that ^ ")")

and get_field obj field =
  match obj with
  | Dict dict -> StringMap.find field dict
  | _ -> failwith "can't get field of this thang"

module Builtins = struct
  let call : builtin_macro =
   fun self args ->
    let f = compile_ast self (StringMap.find "f" args) in
    let args = compile_ast self (StringMap.find "args" args) in
    { ir = Call { f = f.ir; args = args.ir }; new_bindings = StringMap.empty }

  let then' : builtin_macro =
   fun self args ->
    let a = compile_ast self (StringMap.find "a" args) in
    let self_with_new_bindings =
      {
        self with
        data =
          {
            self.data with
            locals = update_locals self.data.locals a.new_bindings;
          };
      }
    in
    match StringMap.find_opt "b" args with
    | Some b ->
        let b = compile_ast self_with_new_bindings b in
        {
          ir = Then (a.ir, b.ir);
          new_bindings = update_locals a.new_bindings b.new_bindings;
        }
    | None -> { ir = Discard a.ir; new_bindings = a.new_bindings }

  let print (args : value) : value =
    match args with
    | String value ->
        print_endline value;
        Void
    | _ -> failwith "print expected a string"

  let if' : builtin_macro =
   fun self args ->
    let cond = compile_ast self (StringMap.find "cond" args) in
    let self_with_new_bindings =
      {
        self with
        data =
          {
            self.data with
            locals = update_locals self.data.locals cond.new_bindings;
          };
      }
    in
    let then' =
      (compile_ast self_with_new_bindings (StringMap.find "then" args)).ir
    in
    let else' =
      match StringMap.find_opt "else" args with
      | Some branch -> (compile_ast self_with_new_bindings branch).ir
      | None -> Void
    in
    {
      ir = If { cond = cond.ir; then_case = then'; else_case = else' };
      new_bindings = StringMap.empty;
    }

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

  let quote : builtin_macro =
   fun self args ->
    let rec impl : ast -> ir = function
      | Complex { def = { name = "unquote"; _ }; values; _ } ->
          let inner = StringMap.find "expr" values in
          Log.trace ("unquoting" ^ Ast.show inner);
          (compile_ast self inner).ir
      | Nothing data -> Const (Ast (Nothing data))
      | Simple token -> Const (Ast (Simple token))
      | Complex { def; values; data } ->
          Ast { def; values = StringMap.map impl values; data }
      | Syntax { value; _ } -> impl value
    in
    let expr = StringMap.find "expr" args in
    Log.trace ("quoting " ^ Ast.show expr);
    { ir = impl expr; new_bindings = StringMap.empty }

  let let' : builtin_macro =
   fun self args ->
    let pattern = StringMap.find "pattern" args in
    let pattern = compile_pattern self (Some pattern) in
    let value = StringMap.find "value" args in
    let value = (compile_ast self value).ir in
    {
      ir = Let { pattern; value };
      new_bindings =
        StringMap.map
          (fun binding : state_local -> Binding binding)
          (pattern_bindings pattern);
    }

  let function_def : builtin_macro =
   fun self args ->
    let args_pattern = StringMap.find_opt "args" args in
    let body = StringMap.find "body" args in
    {
      ir =
        Const
          (Function { captured = self; args_pattern; body; compiled = None });
      new_bindings = StringMap.empty;
    }

  let field_access : builtin_macro =
   fun self args ->
    let obj = StringMap.find "obj" args in
    let obj = (compile_ast self obj).ir in
    let field = StringMap.find "field" args in
    match field with
    | Simple { token = Ident name; _ } ->
        { ir = FieldAccess { obj; name }; new_bindings = StringMap.empty }
    | _ -> failwith "field access must be using an ident"

  let macro = function
    | Function f -> Macro f
    | other -> failwith ("expected a function, got " ^ show other)

  let cmp_fn f =
    dict_fn (fun args ->
        let lhs = StringMap.find "lhs" args in
        let rhs = StringMap.find "rhs" args in
        match (lhs, rhs) with
        | Float lhs, Float rhs -> Bool (f lhs rhs)
        | _ -> failwith "only floats")

  let dbg value = String (show value)

  let all =
    StringMap.of_list
      [
        ("print", BuiltinFn print);
        ("dbg", BuiltinFn dbg);
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
        ("macro", BuiltinFn (single_arg_fn "def" macro));
        ("less", BuiltinFn (cmp_fn ( < )));
        ("less_or_equal", BuiltinFn (cmp_fn ( <= )));
        ("equal", BuiltinFn (cmp_fn ( = )));
        ("not_equal", BuiltinFn (cmp_fn ( <> )));
        ("greater", BuiltinFn (cmp_fn ( > )));
        ("greater_or_equal", BuiltinFn (cmp_fn ( >= )));
      ]
end

let empty () : state =
  let self =
    {
      parent = None;
      data = { locals = StringMap.empty; syntax = Syntax.empty };
    }
  in
  let state =
    {
      self;
      data =
        {
          locals =
            StringMap.map
              (fun value -> Value value)
              (StringMap.union
                 (fun _key _prev value -> Some value)
                 Builtins.all
                 (StringMap.singleton "Self" (Struct self)));
          syntax = Syntax.empty;
        };
    }
  in
  state

let eval (self : state ref) (s : string) ~(filename : string) : value =
  let filename = Span.Filename filename in
  !self.self.data <- !self.data;
  let tokens = Lexer.parse s filename in
  let ast = Ast.parse !self.data.syntax tokens filename in
  let ast = Ast.map (fun span -> { span; ir = None }) ast in
  let result = eval_ast !self ast in
  let rec extend_syntax syntax = function
    | Ast.Syntax { def; value; _ } ->
        extend_syntax (Syntax.add_syntax def syntax) value
    | _ -> syntax
  in
  self :=
    {
      !self with
      data =
        {
          syntax = extend_syntax !self.data.syntax ast;
          locals =
            update_locals !self.data.locals
              (StringMap.map (fun value -> Value value) result.new_bindings);
        };
    };
  result.value

let eval_file (self : state ref) (filename : string) : value =
  let f = open_in filename in
  let contents = really_input_string f (in_channel_length f) in
  close_in f;
  let value = eval self contents ~filename in
  Log.trace ("after " ^ filename ^ " syntax:");
  Log.trace (Syntax.show !self.data.syntax);
  value
