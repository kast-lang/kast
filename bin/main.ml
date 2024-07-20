open Playground;;

Random.self_init ()

module Interpreter = Interpreter.Impl;;

let interpreter = ref (Interpreter.empty ()) in
let rec stdin_loop () =
  print_string "> ";
  let line = read_line () in
  let value = Interpreter.eval interpreter line ~filename:"stdin" in
  print_endline
    (Interpreter.show value ^ " : "
    ^ Interpreter.show_type (Interpreter.type_of_value ~ensure:false value));
  stdin_loop ()
in
List.iter
  (fun file ->
    let value = Interpreter.eval_file interpreter file in
    Interpreter.discard value)
  (List.tl (Array.to_list Sys.argv));
try stdin_loop () with End_of_file -> ()
