open Std
open Kast
open Js_of_ocaml

type file_state = Kast_lsp.Processing.file_state

let yojson_to_js (json : Yojson.Safe.t) : Js.Unsafe.any =
  let json_str = Yojson.Safe.to_string json in
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "JSON.parse")
    [| Js.Unsafe.inject (Js.string json_str) |]

let js_to_yojson (obj : Js.Unsafe.any) : Yojson.Safe.t =
  let json_str : string =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.stringify") [| obj |]
  in
  json_str |> Yojson.Safe.from_string

let semanticTokensProvider =
  object%js
    method getLegend =
      Kast_lsp.Semantic_tokens.legend
      |> Lsp.Types.SemanticTokensLegend.yojson_of_t |> yojson_to_js

    method provideSemanticTokens (state : file_state) =
      match Kast_lsp.Semantic_tokens.run state with
      | Some result ->
          result |> Lsp.Types.SemanticTokens.yojson_of_t |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null
  end

let lsp =
  object%js
    method format (state : file_state) =
      match Kast_lsp.Formatting.run state with
      | Some result ->
          `List (result |> List.map Lsp.Types.TextEdit.yojson_of_t)
          |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null

    method hover (pos : Js.Unsafe.any) (state : file_state) =
      let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
      match Kast_lsp.Hover.hover pos state with
      | Some result -> result |> Lsp.Types.Hover.yojson_of_t |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null

    method rename (pos : Js.Unsafe.any) (newName : string) (state : file_state)
        =
      let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
      match Kast_lsp.Hover.rename pos newName state with
      | Some result ->
          result |> Lsp.Types.WorkspaceEdit.yojson_of_t |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null

    method prepareRename (pos : Js.Unsafe.any) (state : file_state) =
      let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
      match Kast_lsp.Hover.prepare_rename pos state with
      | Some result -> result |> Lsp.Types.Range.yojson_of_t |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null

    method findDefinition (pos : Js.Unsafe.any) (state : file_state) =
      let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
      match Kast_lsp.Hover.find_definition pos state with
      | Some result -> result |> Lsp.Types.Locations.yojson_of_t |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null

    method inlayHints (state : file_state) =
      match Kast_lsp.Inlay_hints.get state with
      | Some result ->
          `List (result |> List.map Lsp.Types.InlayHint.yojson_of_t)
          |> yojson_to_js
      | None -> Js.Unsafe.inject Js.null
  end

let () =
  Js.export "Kast"
    (object%js
       val lsp = lsp
       val semanticTokensProvider = semanticTokensProvider

       method run (source : string) =
         let source : source =
           { contents = source; filename = Special "source" }
         in
         let parsed = Parser.parse source Kast_default_syntax.ruleset in
         match parsed.ast with
         | None -> ()
         | Some ast ->
             let compiler = Compiler.default () in
             (* TODO *)
             let interpreter = compiler.interpreter in
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

       method processFile (uri : string) (source : string) :
           Kast_lsp.Processing.file_state =
         Kast_lsp.Processing.process_file (Lsp.Uri.of_string uri)
           { contents = source; filename = File uri }
    end)
