open Std
open Util
module Rule = Parser.Rule

type rule = Rule.t

let if_rule : rule =
  let priority = 1.0 in
  {
    name = "if";
    priority;
    parts =
      [
        Keyword "if";
        Value { name = Some "cond"; priority = Greater priority };
        Keyword "then";
        Value { name = Some "then"; priority = Greater priority };
        Keyword "else";
        Value { name = Some "else"; priority = GreaterOrEqual priority };
      ];
  }

let apply_rule : rule =
  let priority = 2.0 in
  {
    name = "apply";
    priority;
    parts =
      [
        Value { name = Some "f"; priority = GreaterOrEqual priority };
        Value { name = Some "arg"; priority = Greater priority };
      ];
  }

let scope_rule : rule =
  let priority = 1000.0 in
  {
    name = "scope";
    priority;
    parts = [ Keyword "("; Value { name = None; priority = Any }; Keyword ")" ];
  }

let rules : rule list = [ if_rule; scope_rule; apply_rule ]

let ruleset : Parser.ruleset =
  List.fold_right Parser.RuleSet.add rules Parser.RuleSet.empty

let test ~(source : string) ~(expected : unit) : unit =
  let source : source = { contents = source; filename = "<test>" } in
  let ast = Parser.parse source ruleset in
  match ast with
  | Some ast ->
      Log.debug "Parsed: %a" Ast.print ast;
      failwith "todo test "
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
