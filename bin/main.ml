open Kast;;

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

let eval_files files =
  List.iter
    (fun file ->
      let value = Interpreter.eval_file interpreter file in
      Interpreter.discard value)
    ("std/lib.ks" :: files)
in

let run_repl () = try stdin_loop () with End_of_file -> () in

let cli_args = List.tl (Array.to_list Sys.argv) in

match cli_args with
| [] ->
    eval_files [];
    run_repl ()
| "--repl" :: files ->
    eval_files files;
    run_repl ()
| files -> eval_files files
