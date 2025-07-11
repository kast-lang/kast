open Std
open Kast
open Js_of_ocaml

type file_state = Kast_lsp.Processing.file_state

let global = Kast_lsp.Processing.init []

let current_input : (string -> string Promise.t) ref =
  ref (fun _s -> failwith "input not set")

let cross_js : 'a. (unit -> 'a) -> 'a =
 fun f ->
  try Kast_embedded_std.with_embedded_std f with
  | effect Kast_compiler.Effect.FileStartedProcessing _, k ->
      Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileIncluded _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FileImported _, k -> Effect.Deep.continue k ()

let cross_js_async : 'a. (unit -> 'a) -> 'a Promise.t =
 fun f ->
  try Promise.return (cross_js f)
  with effect Kast_interpreter.Natives.Input s, k ->
    !current_input s |> Promise.bind (fun line : 'a -> Effect.continue k line)

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
      cross_js (fun () ->
          Kast_lsp.Semantic_tokens.legend
          |> Lsp.Types.SemanticTokensLegend.yojson_of_t |> yojson_to_js)

    method provideSemanticTokens (state : file_state) =
      cross_js (fun () ->
          match Kast_lsp.Semantic_tokens.run state with
          | Some result ->
              result |> Lsp.Types.SemanticTokens.yojson_of_t |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)
  end

let lsp =
  object%js
    method format (state : file_state) =
      cross_js (fun () ->
          match Kast_lsp.Formatting.run state with
          | Some result ->
              `List (result |> List.map Lsp.Types.TextEdit.yojson_of_t)
              |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method hover (pos : Js.Unsafe.any) (state : file_state) =
      cross_js (fun () ->
          let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
          match Kast_lsp.Hover.hover pos state with
          | Some result -> result |> Lsp.Types.Hover.yojson_of_t |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method rename (pos : Js.Unsafe.any) (newName : string) (state : file_state)
        =
      cross_js (fun () ->
          let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
          match Kast_lsp.Hover.rename pos newName state with
          | Some result ->
              result |> Lsp.Types.WorkspaceEdit.yojson_of_t |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method prepareRename (pos : Js.Unsafe.any) (state : file_state) =
      cross_js (fun () ->
          let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
          match Kast_lsp.Hover.prepare_rename pos state with
          | Some result -> result |> Lsp.Types.Range.yojson_of_t |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method findDefinition (pos : Js.Unsafe.any) (state : file_state) =
      cross_js (fun () ->
          let pos = pos |> js_to_yojson |> Lsp.Types.Position.t_of_yojson in
          match Kast_lsp.Hover.find_definition pos state with
          | Some result ->
              result |> Lsp.Types.Locations.yojson_of_t |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method inlayHints (state : file_state) =
      cross_js (fun () ->
          match Kast_lsp.Inlay_hints.get state with
          | Some result ->
              `List (result |> List.map Lsp.Types.InlayHint.yojson_of_t)
              |> yojson_to_js
          | None -> Js.Unsafe.inject Js.null)

    method diagnostics (state : file_state) =
      cross_js (fun () ->
          let result = Kast_lsp.Diagnostics.get_for_file global state in
          `List (result |> List.map Lsp.Types.Diagnostic.yojson_of_t)
          |> yojson_to_js)
  end

let () =
  Js.export "Kast"
    (object%js
       val lsp = lsp
       val semanticTokensProvider = semanticTokensProvider

       method run (source : string) =
         cross_js_async (fun () ->
             let source : source =
               { contents = source; uri = Uri.of_string "ocaml:source" }
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
                 ignore value)

       method setOutput (f : string -> unit) =
         cross_js (fun () ->
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
             Format.set_formatter_out_functions out_fns)

       method setInput (f : string -> string Promise.t) = current_input := f

       method processFile (uri : string) (source : string) :
           Kast_lsp.Processing.file_state =
         cross_js (fun () ->
             Kast_lsp.Processing.process_file global
               { contents = source; uri = Uri.of_string uri })
    end)
