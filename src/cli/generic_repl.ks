use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
use (import "../diagnostic.ks").*;
use (import "../readline.ks").*;
use (import "../history.ks").*;
use (import "../rc_str.ks").*;
use std.StringBuilder;

module:

const Repl = (
    module:

    const Args = (
        module:

        const t = newtype {  };

        const default = () -> t => {  };

        const parse = (start_index :: Int32) -> t => (
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                Diagnostic.abort(&format!("unexpected arg \(String.escape(arg))") |> as_str);
                i += 1;
            );
            {  }
        );
    );

    const Line = newtype {
        .raw :: String,
        .parsed :: Parser.Parsed,
    };

    const run_with = (
        .ruleset :: &SyntaxRuleset.t,
        .eval :: Line -> (),
    ) => (
        let mut repl_number :: Int32 = 0;
        let tokenize = (contents :: &str) => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    let () = ();
                ),
            };
            let source = {
                .contents = RcStr.allocate(contents),
                .path = :Special RcStr.from_String(format!("repl\(repl_number)")),
            };
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let mut ranges = ArrayList.new();
            loop (
                let token = &token_stream |> TokenStream.peek;
                if token.shape^ is :Eof then (
                    break;
                );
                &mut token_stream |> TokenStream.advance;
                let range = {
                    .start = token.span.start.string_encoding_index,
                    .end = token.span.end.string_encoding_index,
                };
                &mut ranges |> ArrayList.push_back(range);
            );
            ranges
        );
        let parse = (contents :: &str, .on_error :: Diagnostic.t -> ()) => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    on_error(diagnostic);
                ),
            };
            let source = {
                .contents = RcStr.allocate(contents),
                .path = :Special RcStr.from_String(format!("repl\(repl_number)")),
            };
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            )
        );
        let highlight = (contents :: &str) -> String => (
            let parsed = parse(contents, .on_error = _ => ());
            let mut result = StringBuilder.new();
            with Output = new_output(
                .write = s => (
                    &mut result |> StringBuilder.add_str(s);
                ),
                .indentation_string = "    ",
                .color = true,
            );
            lock_output(() => (
                Highlight.highlight(&parsed, Highlight.new_output(:Terminal));
            ));
            result |> StringBuilder.into_string
        );
        let prompt = output_to_string(() => (
            ansi.with_mode(
                :Dim,
                () => locked_output.write_str("> "),
            )
        ));
        let mut ctrl_c_pressed_times = 0;
        let mut history = History.new_file(".kast_history");
        loop (
            repl_number += 1;
            let line = Readline.read_line(
                .history = &mut history,
                .prompt = &prompt |> as_str,
                .tokenize,
                .highlight,
                .handle_ctrl_c = () => (
                    ctrl_c_pressed_times += 1;
                    if ctrl_c_pressed_times == 1 then (
                        locked_output.write_str("\nPress Ctrl-C again to exit\n");
                        continue;
                    ) else (
                        std.sys.exit(-1);
                    );
                ),
            );
            # reset if Ctrl-C was not pressed
            ctrl_c_pressed_times = 0;
            let parsed = parse(
                &line |> as_str,
                .on_error = diagnostic => (
                    Diagnostic.print_to_stderr(&diagnostic);
                    continue;
                ),
            );
            eval({ .raw = line, .parsed });
        );
    );
);
