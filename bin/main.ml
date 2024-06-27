open Playground;;

let rec main_loop () =
  print_string "> ";
  let line = read_line () in
  let tokens = Lexer.parse line (Filename "stdin") in
  print_endline (String.concat " " (List.of_seq (Seq.map Lexer.show tokens)));
  main_loop ()
in
try main_loop () with End_of_file -> ()
