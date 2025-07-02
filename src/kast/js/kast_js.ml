open Std
open Kast
open Js_of_ocaml

let _ =
  Js.export "Kast"
    (object%js
       method eval (source : string) =
         let source : source =
           { contents = source; filename = Special "source" }
         in
         let parsed = Parser.parse source Kast_default_syntax.ruleset in
         match parsed.ast with
         | None -> println "<none>"
         | Some ast ->
             let interpreter = Interpreter.default () in
             let compiler = Compiler.init ~compile_for:interpreter in
             let expr : expr = Compiler.compile compiler Expr ast in
             let value : value = Interpreter.eval interpreter expr in
             println "%a" Value.print value

       method run (source : string) =
         let source : source =
           { contents = source; filename = Special "source" }
         in
         let parsed = Parser.parse source Kast_default_syntax.ruleset in
         match parsed.ast with
         | None -> ()
         | Some ast ->
             let interpreter = Interpreter.default () in
             let compiler = Compiler.init ~compile_for:interpreter in
             let expr : expr = Compiler.compile compiler Expr ast in
             let value : value = Interpreter.eval interpreter expr in
             ignore value

       method setOutput (f : string -> unit) =
         let out_fns : Format.formatter_out_functions =
           {
             out_string : string -> int -> int -> unit =
               (fun s p n -> f (String.sub s p n));
             out_flush : unit -> unit = (fun () -> ());
             out_newline : unit -> unit = (fun () -> f "\n");
             out_spaces : int -> unit = (fun n -> f (String.make n ' '));
             out_indent : int -> unit = (fun n -> f (String.make n '\t'));
           }
         in
         Format.set_formatter_out_functions out_fns
    end)
