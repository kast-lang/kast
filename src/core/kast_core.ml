module Util = Kast_util
module Token = Kast_token
module Ast = Kast_ast
module Error = Error
module Ty = Ty
module Value = Value
module Expr = Expr
module Assignee = Assignee
module Pattern = Pattern
module Interpreter = Interpreter
module Compiler = Compiler
module Compilable = Compilable
module Plugin = Plugin

let init () =
  Ty.init ();
  Value.init ();
  Expr.init ();
  Assignee.init ();
  Pattern.init ();
  Interpreter.init ();
  Compiler.init ()
