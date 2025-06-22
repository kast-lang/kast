open Std
open Util

let test ~(source : string) ~(expected : unit) : unit =
  let source : source = { contents = source; filename = "<test>" } in
  let ast = Parser.parse source Default_syntax.ruleset in
  match ast with
  | Some ast ->
      Log.debug "Parsed: %a" Ast.print ast;
      failwith "todo test"
  | None -> failwith "nothing was parsed"
;;

Printexc.record_backtrace true;
Log.set_max_level Trace;
try
  test ~source:"Some(Some(String) )" ~expected:();
  test ~source:"if f x then a else b" ~expected:();
  test ~source:"f if cond then a else b" ~expected:()
with Failure s as f ->
  prerr_string s;
  raise f
