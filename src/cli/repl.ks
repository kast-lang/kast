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

use (import "../hir/_lib.ks").*;
use (import "../compiler/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;

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
                Diagnostic.abort("unexpected arg " + String.escape(arg));
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
        .ruleset :: SyntaxRuleset.t,
        .eval :: Line -> (),
    ) => (
        let mut repl_number :: Int32 = 0;
        let tokenize = contents => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    let () = ();
                ),
            };
            let source = {
                .contents,
                .path = :Special ("repl" + to_string(repl_number))
            };
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let mut ranges = ArrayList.new();
            loop (
                let token = &token_stream |> TokenStream.peek;
                if token.shape is :Eof then (
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
        let parse = (contents, .on_error :: Diagnostic.t -> ()) => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    on_error(diagnostic);
                ),
            };
            let source = {
                .contents,
                .path = :Special "repl"
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
        let highlight = contents => (
            let parsed = parse(contents, .on_error = _ => ());
            let mut result = "";
            with Output = new_output(
                .write = s => (
                    result += s;
                ),
                .indentation_string = "    ",
                .color = true,
            );
            Highlight.highlight(&parsed, Highlight.new_output(:Terminal));
            result
        );
        let prompt = output_to_string(
            () => (
                ansi.with_mode(
                    :Dim,
                    () => (@current Output).write("> "),
                )
            )
        );
        let mut ctrl_c_pressed_times = 0;
        let history = History.new_file(".kast_history");
        loop (
            repl_number += 1;
            let line = Readline.read_line(
                .history,
                .prompt,
                .tokenize,
                .highlight,
                .handle_ctrl_c = () => (
                    ctrl_c_pressed_times += 1;
                    if ctrl_c_pressed_times == 1 then (
                        let output = @current Output;
                        output.write("\nPress Ctrl-C again to exit\n");
                        continue;
                    ) else (
                        std.sys.exit(-1);
                    );
                ),
            );
            # reset if Ctrl-C was not pressed
            ctrl_c_pressed_times = 0;
            let parsed = parse(
                line,
                .on_error = diagnostic => (
                    Diagnostic.print(diagnostic);
                    continue;
                ),
            );
            eval({ .raw = line, .parsed });
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset = Common.default_syntax_ruleset();
        let eval = (line :: Line) => with_return (
            with Diagnostic.UnwindableHandler = {
                .unwind_on_error = [T] () -> T => (
                    return
                ),
            };
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    Diagnostic.print(diagnostic);
                ),
            };
            let expr = Compiler.compile[Expr](&line.parsed.ast, .expected_ty = :None);
            let value = Interpreter.eval(&expr);
            if value.shape is :Unit then () else (
                Value.print(&value);
                (@current Output).write("\n");
            );
        );
        Common.with_compiler_interpreter(
            () => run_with(.ruleset, .eval),
        );
    );
);
