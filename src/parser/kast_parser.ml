open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast.T
module Error = Error
module Parsed_part = Parsed_part
module Ruleset = Ruleset
include Effects

type error = Error.t

let error = Error.error

type ruleset = Ruleset.t

type result =
  { ast : Ast.t
  ; ruleset_with_all_new_syntax : ruleset
  ; trailing_comments : Token.comment list
  ; eof : position
  }

let collect_all_new_syntax ast =
  let rec collect_ast (ast : Ast.t) =
    match ast.shape with
    | Ast.Empty -> Seq.empty
    | Ast.Simple _ -> Seq.empty
    | Ast.Complex { rule = _; root } -> collect_group root
    | Ast.Syntax { comments_before = _; tokens = _; mode; value_after } ->
      let after = value_after |> Option.to_seq |> Seq.flat_map collect_ast in
      (match mode with
       | FromScratch -> after
       | Define rule -> Seq.singleton rule |> Seq.append after)
    | Ast.Error _ -> Seq.empty
  and collect_group (group : Ast.group) =
    group.children
    |> Tuple.to_seq
    |> Seq.flat_map (fun (_member, child) -> collect_child child)
  and collect_child (child : Ast.child) =
    match child with
    | Ast.Ast ast -> collect_ast ast
    | Ast.Group group -> collect_group group
  in
  let rules = collect_ast ast |> List.of_seq in
  Ruleset.of_list rules
;;

let parse_with_lexer : Lexer.t -> ruleset -> result =
  fun lexer ruleset ->
  let unused_comments_rev = ref [] in
  let { ast; ruleset_with_all_new_syntax } : Impl.parsed =
    Impl.parse
      { continuation_keywords = StringSet.empty
      ; filter = Any
      ; lexer
      ; ruleset
      ; category = Global
      ; unused_comments_rev
      }
    |> Option.unwrap
  in
  let ruleset_with_all_new_syntax = collect_all_new_syntax ast in
  Log.trace (fun log ->
    log "Parsed %a = %a" Uri.print (Lexer.source lexer).uri Ast.print ast);
  Impl.expect_eof lexer;
  { ast
  ; ruleset_with_all_new_syntax
  ; trailing_comments = !unused_comments_rev |> List.rev
  ; eof = Lexer.position lexer
  }
;;

let parse : source -> ruleset -> result =
  fun source ruleset -> parse_with_lexer (Lexer.init Lexer.default_rules source) ruleset
;;
