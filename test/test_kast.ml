open Kast;;

Printexc.record_backtrace true

let test ~(source : string) ~(expected : token list) : unit =
  let expected = expected @ [ Token.Eof ] in
  let tokens = Lexer.read_all { contents = source; filename = "<test>" } in
  let tokens : token list =
    List.map (fun (spanned : _ spanned) -> spanned.value) tokens
  in
  if tokens <> expected then (
    eprintln "got     : %a" (List.print Token.print) tokens;
    eprintln "expected: %a" (List.print Token.print) expected;
    failwith "test failed")
;;

test ~source:"" ~expected:[];;

test ~source:"hello, world"
  ~expected:
    [
      Token.Ident { raw = "hello" };
      Token.Punct { raw = "," };
      Token.Ident { raw = "world" };
    ]
