type _ compiled_kind =
  | Assignee : int compiled_kind
  | Expr : string compiled_kind

and compiled = Compiled : 'a. 'a compiled_kind * 'a -> compiled

let () =
  let path = "/home/kuvi/projects/doom" in
  let uri = Uri.make ~scheme:"file" ~host:"" ~path () in
  print_endline (Uri.to_string uri)
