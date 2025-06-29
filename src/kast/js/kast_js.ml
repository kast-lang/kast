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
    end)
