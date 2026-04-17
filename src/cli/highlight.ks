use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../position.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../highlight.ks").*;

const root_scope = @current_scope;

module:

const Highlight = (
    module:

    const Args = (
        module:

        const t = newtype {
            .ruleset :: Option.t[String],
            .paths :: ArrayList.t[String],
            .mode :: root_scope.Highlight.OutputMode,
        };

        const parse = (
            start_index :: Int32,
            .fix_syntax :: Option.t[Common.Syntax],
        ) -> t => (
            let mut @"syntax" = fix_syntax;
            let mut paths = ArrayList.new();
            let mut mode = :Terminal;
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--ruleset" and &@"syntax" |> Option.is_none then (
                    @"syntax" = :Some {
                        .ruleset = std.sys.argv_at(i + 1),
                        .ext = :None,
                    };
                    i += 2;
                    continue;
                );
                if arg == "--mode" then (
                    mode = String.parse(std.sys.argv_at(i + 1));
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg_for_syntax(arg, .@"syntax"));
                i += 1;
            );
            {
                .ruleset = @"syntax" |> Option.map(s => s.ruleset),
                .paths,
                .mode,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = args.ruleset |> Option.unwrap_or("kast:///std/syntax.ks");
        let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);

        let output = root_scope.Highlight.new_output(args.mode);
        let process = (path :: SourcePath) => (
            let source = Source.read(path);
            let entire_source_span = (
                let start = Position.beginning();
                let mut end = start;
                for c in source.contents |> String.iter do (
                    &mut end |> Position.advance(c);
                );
                {
                    .start,
                    .end,
                    .path = source.path,
                }
            );
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span,
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            root_scope.Highlight.highlight(&parsed, output);
            (@current Output).write("\n");
            # Reset for next file
            output.move_to(Position.beginning(), .print_whitespace = false);
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
    );
);
