open Playground;;

let syntax = ref (Syntax.make_syntax []) in
let rec eval = function
  | Ast.Syntax { def; value } ->
      syntax := Syntax.add_syntax def !syntax;
      eval value
  | _ -> ()
in
let rec stdin_loop () =
  print_string "> ";
  let line = read_line () in
  let tokens = Lexer.parse line (Filename "stdin") in

  (* Log.info "tokens:";
     Log.info (String.concat " " (List.of_seq (Seq.map Lexer.show tokens))); *)
  let ast = Ast.parse !syntax tokens in
  eval ast;
  Log.info "ast:";
  prerr_endline (Ast.show ast);
  stdin_loop ()
in
let eval_file () =
  let filename = Array.get Sys.argv 1 in
  let f = open_in filename in
  let contents = really_input_string f (in_channel_length f) in
  close_in f;

  let tokens = Lexer.parse contents (Filename filename) in
  let ast = Ast.parse !syntax tokens in
  (* prerr_endline (Ast.show ast); *)
  eval ast
in
if Array.length Sys.argv > 1 then eval_file ();
try stdin_loop () with End_of_file -> ()
