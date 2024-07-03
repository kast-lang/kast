type ident = { name : string; def : Span.span }
type node = Void | Ident of ident
type t = node
type ast_data = { span : Span.span }
type ast = ast_data Ast.node

let compile (ast : ast) : node =
  match ast with
  | Nothing _ -> Void
  | Simple { token; _ } -> failwith "todo"
  | Complex { def; values; _ } -> failwith "todo"
  | Syntax { def; value; _ } -> failwith "todo"
