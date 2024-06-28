open Playground;;

(* call: left 50 = f args *)
(* add: left 20 = a "+" b *)
(* negate: left 100 = "-" x *)
(* if/then/else: left 10 = "if" cond "then" then "else" else *)
let syntax =
  Syntax.make_syntax
    [
      (* {
           name = "call";
           assoc = Left;
           priority = 50;
           parts = [ Binding "f"; Binding "args" ];
         }; *)
      {
        name = "add";
        assoc = Left;
        priority = 20;
        parts = [ Binding "lhs"; Keyword "+"; Binding "rhs" ];
      };
      {
        name = "sub";
        assoc = Left;
        priority = 20;
        parts = [ Binding "lhs"; Keyword "-"; Binding "rhs" ];
      };
      {
        name = "mul";
        assoc = Left;
        priority = 40;
        parts = [ Binding "lhs"; Keyword "*"; Binding "rhs" ];
      };
      {
        name = "div";
        assoc = Left;
        priority = 40;
        parts = [ Binding "lhs"; Keyword "/"; Binding "rhs" ];
      };
      {
        name = "mod";
        assoc = Left;
        priority = 40;
        parts = [ Binding "lhs"; Keyword "%"; Binding "rhs" ];
      };
      {
        name = "pow";
        assoc = Right;
        priority = 60;
        parts = [ Binding "lhs"; Keyword "^"; Binding "rhs" ];
      };
      {
        name = "parens";
        assoc = Left;
        priority = Int.max_int;
        parts = [ Keyword "("; Binding "x"; Keyword ")" ];
      };
    ]
in
Log.debug ("syntax:\n" ^ Syntax.show syntax);

let rec main_loop () =
  print_string "> ";
  let line = read_line () in
  let tokens = Lexer.parse line (Filename "stdin") in

  (* Log.info "tokens:";
     Log.info (String.concat " " (List.of_seq (Seq.map Lexer.show tokens))); *)
  let ast = Ast.parse syntax tokens in
  Log.info "ast:";
  prerr_endline (Ast.show ast);
  main_loop ()
in
try main_loop () with End_of_file -> ()
