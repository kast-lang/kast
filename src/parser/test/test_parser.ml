open Std
open Util

type expected =
  | Simple of string
  | Complex of { name : string; children : expected Tuple.t }

let rec print_expected fmt = function
  | Simple s -> fprintf fmt "%S" s
  | Complex { name; children } ->
      fprintf fmt "%S %a" name (Tuple.print print_expected) children

let rec matches (ast : Ast.t) (expected : expected) : bool =
  match (ast, expected) with
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

let test ~(source : string) ~(expected : expected) : unit =
  let source : source = { contents = source; filename = "<test>" } in
  let ast = Parser.parse source Default_syntax.ruleset in
  match ast with
  | Some ast ->
      if not (matches ast expected) then (
        Log.error "@[<v>Parsed: %a@]" Ast.print ast;
        Log.error "@[<v>Expected %a@]" print_expected expected;
        failwith "Test failed")
  | None -> failwith "nothing was parsed"
;;

Printexc.record_backtrace true;
Log.set_max_level Debug;
try
  test ~source:"Some(Some(String) )"
    ~expected:
      (Complex
         {
           name = "apply";
           children =
             Tuple.make []
               [
                 ("f", Simple "Some");
                 ( "arg",
                   Complex
                     {
                       name = "scope";
                       children =
                         Tuple.make
                           [
                             Complex
                               {
                                 name = "apply";
                                 children =
                                   Tuple.make []
                                     [
                                       ("f", Simple "Some");
                                       ( "arg",
                                         Complex
                                           {
                                             name = "scope";
                                             children =
                                               Tuple.make [ Simple "String" ] [];
                                           } );
                                     ];
                               };
                           ]
                           [];
                     } );
               ];
         });
  test ~source:"if f x then a else b"
    ~expected:
      (Complex
         {
           name = "if";
           children =
             Tuple.make []
               [
                 ( "cond",
                   Complex
                     {
                       name = "apply";
                       children =
                         Tuple.make []
                           [ ("f", Simple "f"); ("arg", Simple "x") ];
                     } );
                 ("then", Simple "a");
                 ("else", Simple "b");
               ];
         });
  test ~source:"f if cond then a else b"
    ~expected:(Simple "should not even parse")
with Failure s as f ->
  prerr_string s;
  raise f
