use (import "../diagnostic.ks").*;
use (import "../token_stream.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../parser.ks").*;
use (import "../compiler/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;
use (import "../hir/_lib.ks").*;

module:

const Common = (
    module:

    const Syntax = newtype {
        .ruleset :: String,
        .ext :: Option.t[String],
    };

    const path_arg_for_syntax = (path :: String, .@"syntax" :: Option.t[Syntax]) -> String => (
        let ext = match @"syntax" with (
            | :None => :Some "ks"
            | :Some s => s.ext
        );
        path_arg(path, .ext)
    );

    const path_arg = (path :: String, .ext :: Option.t[String]) -> String => (
        let fail = () => (
            let mut message = "Expected a path";
            if ext is :Some ext then (
                message += " with ." + ext + " extension";
            );
            message += ", got " + String.escape(path);
            Diagnostic.abort(message)
        );
        if String.length(path) == 0 then fail();
        if path |> String.at(0) == '-' then fail();
        let last_dot = path |> String.last_index_of('.');
        if last_dot < 0 then fail();
        let actual_ext = path |> String.substring_from(last_dot + 1);
        if ext is :Some ext then (
            if actual_ext != ext then fail();
        );
        path
    );

    const Args = (
        module:

        const ColorMode = newtype (
            | :Auto
            | :Always
            | :Never
        );

        const t = newtype {
            .output_mode :: (
                | :Human
                | :Json
            ),
            .stop_on_error :: Bool,
            .color :: ColorMode,
        };

        const default = () -> Args.t => {
            .output_mode = :Human,
            .stop_on_error = true,
            .color = :Auto,
        };

        const parse_arg = (
            args :: &mut Args.t,
            arg_idx :: &mut Int32,
        ) => with_return (
            let arg = std.sys.argv_at(arg_idx^);
            if arg == "--output-mode" then (
                let mode = std.sys.argv_at(arg_idx^ + 1);
                let mode = if mode == "human" then (
                    :Human
                ) else if mode == "json" then (
                    :Json
                ) else (
                    Diagnostic.abort("Unknown output mode " + String.escape(mode))
                );
                args^.output_mode = mode;
                arg_idx^ += 2;
                return;
            );
            if arg == "--continue-on-error" then (
                args^.stop_on_error = false;
                arg_idx^ += 1;
                return;
            );
            if arg == "--color" then (
                let value = std.sys.argv_at(arg_idx^ + 1);
                args^.color = if value == "always" then (
                    :Always
                ) else if value == "never" then (
                    :Never
                ) else if value == "auto" then (
                    :Auto
                ) else (
                    Diagnostic.abort("Unexpected value for --color. Use always/never/auto")
                );
                arg_idx^ += 2;
                return;
            );
            Diagnostic.abort("Unexpected arg " + arg);
        );
    );

    const output_color = (isatty :: Bool, color :: Args.ColorMode) -> Bool => (
        match color with (
            | :Always => true
            | :Never => false
            | :Auto => isatty
        )
    );

    const default_syntax_ruleset = () -> SyntaxRuleset.t => (
        let ruleset_path = "kast:///std/syntax.ks";
        let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const with_compiler_interpreter = [R] (f :: () -> R) -> R => (
        let ruleset = default_syntax_ruleset();
        let { .compiler, .state = compiler_state, .scope } = Compiler.init();
        with Compiler.Context = compiler;
        with Compiler.StateContext = compiler_state;
        with Compiler.Scope.Context = scope;
        let { .state = interpreter_state, .scope } = Interpreter.init();
        with Interpreter.StateContext = interpreter_state;
        with Interpreter.Scope.Context = scope;
        (
            # Prelude
            let prelude_path = "kast:///std/prelude.ks";
            let source = Source.read(SourcePath.parse(prelude_path));
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );
            Compiler.compile[Expr](&parsed.ast, .expected_ty = :None);
        );
        f()
    );
);
