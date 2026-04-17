use (import "./common.ks").*;
use (import "./state.ks").*;
use (import "./init.ks").*;
use (import "./hover.ks").*;
use (import "./selection_range.ks").*;
use (import "./semantic_tokens.ks").*;
use (import "./formatting.ks").*;
use (import "./diagnostics.ks").*;

module:

const Lsp = (
    module:

    const CliArgs = (
        module:

        const t = newtype {  };

        const parse = start_index -> t => (
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                Diagnostic.abort("Unexpected arg " + String.escape(arg));
                i += 1;
            );
            {  }
        );
    );

    const on_request = (
        state :: &mut State,
        request :: Json.t,
    ) -> Json.t => with_return (
        let :Object request_fields = request;
        let &(:String method) = &request_fields |> OrdMap.get("method") |> Option.unwrap;
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received request ");
                output.write(method);
            )
        );
        if method == "initialize" then (
            return state |> initialize(request);
        );
        if method == "textDocument/selectionRange" then (
            return state |> selection_range(request);
        );
        if method == "textDocument/hover" then (
            return state |> hover(request);
        );
        if method == "textDocument/semanticTokens/full" then (
            return state |> semantic_tokens.full(request);
        );
        if method == "textDocument/formatting" then (
            return state |> formatting.format(request);
        );
        if method == "textDocument/diagnostic" then (
            return state |> diagnostics.document(request);
        );
        if method == "workspace/diagnostic" then (
            return state |> diagnostics.workspace(request);
        );
        panic("TODO respond to " + method)
    );
    const on_notification = (state :: &mut State, notification :: Json.t) -> () => with_return (
        let :Object fields = notification;
        let &(:String method) = &fields |> OrdMap.get("method") |> Option.unwrap;
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received notification ");
                output.write(method);
            )
        );
        if method == "textDocument/didOpen" then (
            return state |> did_open(notification);
        );
        if method == "textDocument/didChange" then (
            return state |> did_change(notification);
        );
    );

    const run = (arg :: CliArgs.t) => (
        (@current Stdout).color = false;
        (@current Stderr).color = false;
        let kast_syntax_file :: Source = Source.read(SourcePath.parse("kast:///std/syntax.ks"));
        let minikast_syntax_file :: Source = Source.read(SourcePath.parse("kast:///mini/syntax.ks"));
        let get_syntax = source => (
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            SyntaxParser.parse_syntax_ruleset(&mut token_stream)
        );
        let mut state :: State = {
            .@"syntax" = {
                .kast = get_syntax(kast_syntax_file),
                .minikast = get_syntax(minikast_syntax_file),
            },
            .files = OrdMap.new(),
        };
        JsonRpc.run(
            JsonRpc.stdio(),
            {
                .on_request = request => with_return (
                    let abort_handler = [T] (msg :: String) -> T => (
                        return :Error msg
                    );
                    with Diagnostic.AbortHandler = abort_handler;
                    :Ok on_request(&mut state, request)
                ),
                .on_notification = notification => (
                    on_notification(&mut state, notification)
                ),
            }
        )
    );
);
