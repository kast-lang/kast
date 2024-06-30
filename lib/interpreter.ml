type state = { syntax : Syntax.syntax }
type value = Todo of Ast.value | Void

let show = function
  | Todo ast -> Ast.show ast
  | Void -> "void"

let empty () : state = { syntax = Syntax.empty }

let eval_ast (self : state) (ast : Ast.value) : value =
  match ast with
  | Nothing -> Void
  | Simple { value = token; span = _ } -> Todo ast
  | Complex syntax -> Todo ast
  | Syntax { def; value } -> Todo ast

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
