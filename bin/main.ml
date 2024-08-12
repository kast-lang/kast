open Kast
open Show
open Interpreter
open Compiler;;

Random.self_init ();

let interpreter = ref (Interpreter.default_state ()) in

let rec stdin_loop () =
  print_string "> ";
  let line = read_line () in
  let value = Interpreter.eval interpreter line ~filename:"stdin" in
  print_endline
    (show value ^ " :: " ^ show_type (type_of_value ~ensure:true value));
  stdin_loop ()
in

let eval_files files =
  List.iter
    (fun file ->
      let value = eval_file interpreter ~filename:file in
      Interpreter.discard value)
    files
in

let run_repl () = try stdin_loop () with End_of_file -> () in

let cli_args = List.tl (Array.to_list Sys.argv) in

match cli_args with
| [] ->
    eval_files [];
    run_repl ()
| [ "--to-js"; file ] ->
    print_endline
    @@ (Javascript.compile_value
      @@ Ir (compile_file interpreter ~filename:file).ir)
         .code
| "--repl" :: files ->
    eval_files files;
    run_repl ()
| files -> eval_files files
