open Std
open Kast_util
module Lsp = Linol_lsp
module Token = Kast_token
module Ast = Kast_ast

type linecol =
  { line : int
  ; column : int
  }

let linecol (pos : position) : linecol = { line = pos.line; column = pos.column }

type token_shape =
  | Keyword of Token.Shape.t
  | Value of Token.Shape.t
  | Comment of Token.Shape.comment
  | Unknown of Token.Shape.t

type token =
  { token : token_shape
  ; span : span
  }

let rec collect_parts (parts : Ast.part list) : token Seq.t =
  parts
  |> List.to_seq
  |> Seq.flat_map (function
    | Ast.Value ast -> collect ast
    | Ast.Keyword token ->
      List.to_seq [ { token = Keyword token.shape; span = token.span } ]
    | Ast.Comment comment ->
      List.to_seq [ { token = Comment comment.shape; span = comment.span } ]
    | Ast.Group group -> collect_parts group.parts)

and collect : Ast.t -> token Seq.t =
  fun { shape; span = _ } ->
  match shape with
  | Ast.Error { parts } -> collect_parts parts
  | Ast.Simple { comments_before; token } ->
    Seq.append
      (comments_before
       |> List.to_seq
       |> Seq.map (fun (comment : Token.comment) ->
         { token = Comment comment.shape; span = comment.span }))
      (List.to_seq [ { token = Value token.shape; span = token.span } ])
  | Ast.Complex { root; _ } -> collect_parts root.parts
  | Ast.Syntax { value_after; tokens; _ } ->
    Seq.append
      (tokens
       |> List.to_seq
       |> Seq.map (fun (token : Token.t) ->
         { token = Unknown token.shape; span = token.span }))
      (value_after |> Option.to_seq |> Seq.flat_map collect)
;;

let legend =
  Lsp.Types.SemanticTokensLegend.create
    ~tokenTypes:
      [ "namespace"
      ; "class"
      ; "enum"
      ; "interface"
      ; "struct"
      ; "typeParameter"
      ; "type"
      ; "parameter"
      ; "variable"
      ; "property"
      ; "enumMember"
      ; "decorator"
      ; "event"
      ; "function"
      ; "method"
      ; "macro"
      ; "label"
      ; "comment"
      ; "string"
      ; "keyword"
      ; "number"
      ; "regexp"
      ; "operator"
      ]
    ~tokenModifiers:
      [ "declaration"
      ; "definition"
      ; "readonly"
      ; "static"
      ; "deprecated"
      ; "abstract"
      ; "async"
      ; "modification"
      ; "documentation"
      ; "defaultLibrary"
      ]
;;

let options : Lsp.Types.SemanticTokensRegistrationOptions.t =
  Lsp.Types.SemanticTokensRegistrationOptions.create ~full:(`Bool true) ~legend ()
;;

let run ({ parsed; _ } : Processing.file_state) : Lsp.Types.SemanticTokens.t option =
  Log.trace (fun log -> log "got semantic tokens request");
  match parsed with
  | None -> None
  | Some { ast; trailing_comments; eof = _ } ->
    let data =
      let prev_pos = ref Position.beginning in
      let tokens =
        Seq.append
          (ast |> Option.to_seq |> Seq.flat_map (fun ast -> ast |> collect))
          (trailing_comments
           |> List.to_seq
           |> Seq.map (fun (comment : Token.comment) : token ->
             { token = Comment comment.shape; span = comment.span }))
      in
      tokens
      |> Seq.flat_map (fun ({ token; span } : token) ->
        let pos = ref <| linecol span.start in
        Seq.of_dispenser (fun () ->
          if !pos = linecol span.finish
          then None
          else (
            let next_pos =
              if !pos.line = span.finish.line
              then linecol span.finish
              else { line = !pos.line + 1; column = 1 }
            in
            (* we dont use index anyway *)
            let fakepos (pos : linecol) : position =
              { line = pos.line; column = pos.column; index = 0 }
            in
            let span : span =
              { start = fakepos !pos; finish = fakepos next_pos; uri = span.uri }
            in
            pos := next_pos;
            Some span))
        |> Seq.map (fun span : token -> { token; span }))
      |> Seq.flat_map (fun ({ token; span } : token) ->
        let deltaLine = span.start.line - !prev_pos.line in
        let deltaStartChar =
          if deltaLine = 0
          then span.start.column - !prev_pos.column
          else span.start.column - Position.beginning.column
        in
        let length =
          if span.start.line = span.finish.line
          then span.finish.column - span.start.column
          else 1000 (* to the end of line :) *)
        in
        let tokenType =
          match token with
          | Keyword _ -> Some 19 (* keyword *)
          | Value token ->
            (match token with
             | String _ -> Some 18 (* string *)
             | Number _ -> Some 20
             | _ -> None)
          | Comment _ -> Some 17
          | Unknown _ -> Some 15
        in
        let tokenModifiers = 0 in
        let data =
          tokenType
          |> Option.map (fun tokenType ->
            [ deltaLine; deltaStartChar; length; tokenType; tokenModifiers ])
        in
        (match data with
         | Some data ->
           Log.trace (fun log ->
             log "@[<h>data: %a %a@]" (List.print Int.print) data Span.print span);
           prev_pos := span.start
         | None -> ());
        let data = data |> Option.value ~default:[] in
        List.to_seq data)
      |> Array.of_seq
    in
    let tokens = Lsp.Types.SemanticTokens.create ~data ?resultId:None () in
    Log.trace (fun log -> log "replied with semantic tokens");
    Some tokens
;;
