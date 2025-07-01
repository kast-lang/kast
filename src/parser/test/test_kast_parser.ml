open Std
open Kast_util
module Token = Kast_token
module Ast = Kast_ast
module Lexer = Kast_lexer
module Parser = Kast_parser

type expected = Kast_simple_syntax.ast

let rec matches (ast : Ast.t) (expected : expected) : bool =
  match (ast.shape, expected) with
  | Simple { token; _ }, Simple expected ->
      Token.raw token |> Option.get = expected
  | Simple _, _ -> false
  | ( Complex { rule; root },
      Complex { name = expected_name; children = expected_children } ) -> (
      (* TODO make tests work with groups *)
      let children = root.children |> Tuple.map Ast.Child.expect_ast in
      let rule_name =
        rule.name
        |> String.strip_prefix ~prefix:"core:"
        |> Option.value ~default:rule.name
      in
      rule_name = expected_name
      &&
      try
        Tuple.zip_order_a children expected_children
        |> Tuple.to_seq
        |> Seq.for_all (fun (_member, (child, expected_child)) ->
               matches child expected_child)
      with Invalid_argument _ -> false)
  | Complex _, _ -> false
  | Syntax _, _ -> false

let test_should_fail ?(ruleset : Parser.ruleset option) (source : string) : unit
    =
  try
    let { ast; trailing_comments = _; eof = _ } : Parser.result =
      Parser.parse
        { contents = source; filename = Special "test" }
        (ruleset |> Option.value ~default:Kast_default_syntax.ruleset)
    in
    Log.error "Parsed: %a" (Option.print Ast.print) ast;
    failwith "Parse was supposed to fail"
  with
  | Parser.Error { msg; span = _ } ->
      Log.trace "Test properly failed: %a" (fun fmt () -> msg fmt) ()
  | Lexer.Error f ->
      Log.trace "Test properly failed: %a" (fun fmt () -> f fmt) ()

let test ~(source : string) ~(expected : string)
    ?(ruleset : Parser.ruleset option) () : unit =
  let expected =
    Kast_simple_syntax.parse { contents = expected; filename = Special "test" }
    |> Option.get
  in
  let { ast; trailing_comments = _; eof = _ } : Parser.result =
    Parser.parse
      { contents = source; filename = Special "test" }
      (ruleset |> Option.value ~default:Kast_default_syntax.ruleset)
  in
  match ast with
  | Some ast ->
      if not (matches ast expected) then (
        Log.error "Parsed: %a" Ast.print ast;
        Log.error "Expected %a" Kast_simple_syntax.print expected;
        failwith "Test failed")
  | None -> failwith "nothing was parsed"
;;

Printexc.record_backtrace true;
Log.set_max_level Debug;
(let then_rule p = make_string "then %d wrap never = _ \";\" _" p in
 let eq_rule p = make_string "eq %d wrap never = _ \"=\" _ " p in
 test
   ~ruleset:(Parser.RuleSet.parse_list [ then_rule 0; eq_rule 1 ])
   ~source:"a=1;b=2" ~expected:"then( eq( a, 1 ), eq( b, 2 ) )" ();
 test_should_fail
   ~ruleset:(Parser.RuleSet.parse_list [ then_rule 2; eq_rule 1 ])
   "a=1;b=2");
test ~source:"Some(Some(String))"
  ~expected:
    "apply(f = Some, arg = scope( apply( f = Some, arg = scope( String ) ) ) )"
  ();
test ~source:"if f x then a else b"
  ~expected:"if( cond = apply( f = f, arg = x ), then = a, else = b )" ();
test_should_fail "f if cond then a else b"
