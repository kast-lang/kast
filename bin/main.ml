open Playground;;

let interpreter = ref (Interpreter.empty ()) in
let rec stdin_loop () =
  print_string "> ";
  let line = read_line () in
  let value = Interpreter.eval interpreter line ~filename:"stdin" in
  print_endline (Interpreter.show value);
  stdin_loop ()
in
List.iter
  (fun file ->
    let value = Interpreter.eval_file interpreter file in
    match value with
    | Void -> ()
    | _ ->
        print_endline
          ("expected void in " ^ file ^ ", got " ^ Interpreter.show value))
  (List.tl (Array.to_list Sys.argv));
try stdin_loop () with End_of_file -> ()
