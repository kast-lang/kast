open Std
open Util

type expected = Simple_syntax.ast

let rec matches (ast : Ast.t) (expected : expected) : bool =
  match (ast.kind, expected) with
  | Simple { token }, Simple expected ->
      Lexer.Token.raw token |> Option.get = expected
  | Simple _, _ -> false
  | ( Complex { name; children },
      Complex { name = expected_name; children = expected_children } ) -> (
      name = expected_name
      &&
      try
        Tuple.zip_order_a children expected_children
        |> Tuple.to_seq
        |> Seq.for_all (fun (_member, (child, expected_child)) ->
               matches child expected_child)
      with Invalid_argument _ -> false)
  | Complex _, _ -> false

let test ~(source : string) ~(expected : string) : unit =
  let expected =
    Simple_syntax.parse { contents = expected; filename = "<test>" }
    |> Option.get
  in
  let ast =
    Parser.parse
      { contents = source; filename = "<test>" }
      Default_syntax.ruleset
  in
  match ast with
  | Some ast ->
      if not (matches ast expected) then (
        Log.error "@[<v>Parsed: %a@]" Ast.print ast;
        Log.error "@[<v>Expected %a@]" Simple_syntax.print expected;
        failwith "Test failed")
  | None -> failwith "nothing was parsed"
;;

Printexc.record_backtrace true;
Log.set_max_level Debug;
try
  test ~source:"Some(Some(String))"
    ~expected:
      "apply(f = Some, arg = scope( apply( f = Some, arg = scope( String ) ) ) )";
  test ~source:"if f x then a else b"
    ~expected:"if( cond = apply( f = f, arg = x ), then = a, else = b )";
  test ~source:"f if cond then a else b" ~expected:"\"should not even parse\""
with Failure s as f ->
  prerr_string s;
  raise f
