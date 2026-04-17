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
                        .ruleset = std.sys.argv_at(i + 1),
                        .ext = :None,
                    };
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg_for_syntax(arg, .@"syntax"));
                i += 1;
            );
            {
                .ruleset_path = @"syntax" |> Option.map(s => s.ruleset),
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = args.ruleset_path |> Option.unwrap_or("kast:///std/syntax.ks");
        ansi.with_mode(
            :Bold,
            () => (@current Output).write("Parsing syntax rules from " + ruleset_path + "\n\n"),
        );
        let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let process = (path :: SourcePath) => (
            ansi.with_mode(
                :Bold,
                () => (@current Output).write("Parsing " + to_string(path) + "\n\n"),
            );
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            Ast.print(&parsed.ast);
            (@current Output).write("\n");
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
    );
);
