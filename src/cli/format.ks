use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../highlight.ks").*;
use (import "../format.ks").*;

const root_scope = @current_scope;

module:

const Format = (
    module:

    const Args = (
        module:

        const t = newtype {
            .ruleset :: Option.t[String],
            .paths :: ArrayList.t[String],
            .highlight :: Option.t[Highlight.OutputMode],
            .inplace :: Bool,
        };

        const parse = (
            start_index :: Int32,
            .fix_syntax :: Option.t[Common.Syntax],
        ) -> t => (
            let mut @"syntax" = fix_syntax;
            let mut paths = ArrayList.new();
            let mut i = start_index;
            let mut highlight = :Some :Terminal;
            let mut inplace = false;
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
                if arg == "--highlight" then (
                    let mode = std.sys.argv_at(i + 1);
                    if mode == "none" then (
                        highlight = :None;
                    ) else (
                        highlight = :Some String.parse(mode);
                    );
                    i += 2;
                    continue;
                );
                if arg == "--inplace" then (
                    inplace = true;
                    i += 1;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg_for_syntax(arg, .@"syntax"));
                i += 1;
            );
            {
                .ruleset = @"syntax" |> Option.map(s => s.ruleset),
                .paths,
                .highlight,
                .inplace,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        # TODO because we mutate ruleset when parsing actually but should not be the case
        let get_ruleset = () => (
            let ruleset_path = args.ruleset |> Option.unwrap_or("kast:///std/syntax.ks");
            let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path)));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            SyntaxParser.parse_syntax_ruleset(&mut token_stream)
        );
        let process = (path :: SourcePath) => (
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset = get_ruleset(),
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            if args.inplace then (
                let formatted = root_scope.Format.format_to_string(&parsed);
                let path = match path |> SourcePath.file_path with (
                    | :Some path => path
                    | :None => Diagnostic.abort("Inplace formatting is only available given file path")
                );
                # TODO std.fs.write_file
                @native "(await import('fs')).writeFileSync(\(path), \(formatted))";
            ) else match args.highlight with (
                | :None => root_scope.Format.format(&parsed, @current Output)
                | :Some highlight_mode => (
                    let source = {
                        .contents = root_scope.Format.format_to_string(&parsed),
                        .path = :Special "<formatted>",
                    };
                    let mut lexer = Lexer.new(source);
                    let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                    let parsed = Parser.parse(
                        .ruleset = get_ruleset(),
                        .entire_source_span = Source.entire_span(&source),
                        .path = source.path,
                        .token_stream = &mut token_stream,
                    );
                    Highlight.highlight(&parsed, Highlight.new_output(highlight_mode));
                )
            );
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
    );
);
