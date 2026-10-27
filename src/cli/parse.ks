use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;

module:

const Parse = (
    module:

    const Args = (
        module:

        const t = newtype {
            .ruleset_path :: Option.t[String],
            .paths :: ArrayList.t[String],
        };

        const parse = (
            start_index :: Int32,
            .fix_syntax :: Option.t[Common.Syntax],
        ) -> t => (
            let mut @"syntax" = fix_syntax;
            let mut paths = ArrayList.new();
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--ruleset" and &@"syntax" |> Option.is_none then (
                    if i + 1 >= std.sys.argc() then (
                        Diagnostic.abort("Expected ruleset path");
                    );
                    @"syntax" = :Some {
                        .ruleset = String.from_str(std.sys.argv_at(i + 1)),
                        .ext = :None,
                    };
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg_for_syntax(arg, .@"syntax" = &@"syntax" |> Option.as_ref));
                i += 1;
            );
            {
                .ruleset_path = @"syntax" |> Option.map(s => s.ruleset),
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = match args.ruleset_path with (
            | :Some ref path => path |> as_str
            | :None => "kast:///std/syntax.ks"
        );
        ansi.with_mode(
            :Bold,
            () => (
                write_str("Parsing syntax rules from ");
                write_str(ruleset_path);
                write_str("\n\n");
            ),
        );
        let mut lexer = Lexer.new(Source.read(&SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let mut ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let process = (path :: &SourcePath) => (
            ansi.with_mode(
                :Bold,
                () => (
                    write_str("Parsing ");
                    SourcePath.print(path);
                    write_str("\n\n");
                ),
            );
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset = &ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            match common_args.output_mode with (
                | :None => ()
                | :Human => (
                    Ast.print(&parsed.ast);
                    write_str("\n");
                )
            );
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(&(:Stdin));
        );
        for path in &args.paths |> ArrayList.iter do (
            process(&SourcePath.parse(path |> as_str));
        );
    );
);
