open Playground;;

let rec main_loop () =
  print_string "> ";
  let line = read_line () in
  let tokens = Lexer.parse line in
  print_endline
    ("tokens: " ^ String.concat " " (List.of_seq (Seq.map Lexer.show tokens)));
  let ast = Ast.parse tokens in
  print_endline ("ast: " ^ Ast.show ast);
  let value = Interpreter.eval ast in
  print_endline (Interpreter.show value);
  main_loop ()
in
main_loop ()
