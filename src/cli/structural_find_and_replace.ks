use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../position.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
use (import "../structural_find_and_replace.ks").*;
use std.collections.OrdMap;

const root_scope = @current_scope;

module:

const StructuralFindAndReplace = (
    module:

    const Args = (
        module:

        const t = newtype {
            .ruleset :: Option.t[String],
            .paths :: ArrayList.t[String],
            .pattern :: String,
            .replace :: Option.t[String],
            .replace_ruleset :: Option.t[String],
            .inplace :: Bool,
        };

        const parse = (
            start_index :: Int32,
            .fix_syntax :: Option.t[Common.Syntax],
        ) -> t => (
            let mut @"syntax" = fix_syntax;
            let mut paths = ArrayList.new();
            let mut pattern = :None;
            let mut replace = :None;
            let mut replace_ruleset = :None;
            let mut inplace = false;
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
                if arg == "--pattern" then (
                    pattern = :Some std.sys.argv_at(i + 1);
                    i += 2;
                    continue;
                );
                if arg == "--replace" then (
                    replace = :Some std.sys.argv_at(i + 1);
                    i += 2;
                    continue;
                );
                if arg == "--replace-ruleset" then (
                    replace_ruleset = :Some std.sys.argv_at(i + 1);
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
            let pattern = pattern |> Option.unwrap_or_else(() => Diagnostic.abort("missing --pattern arg"));
            {
                .ruleset = @"syntax" |> Option.map(s => s.ruleset),
                .paths,
                .pattern,
                .replace,
                .replace_ruleset,
                .inplace,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = args.ruleset |> Option.unwrap_or("kast:///std/syntax.ks");
        let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let replace_ruleset = match args.replace_ruleset with (
            | :None => ruleset
            | :Some path => (
                let mut lexer = Lexer.new(Source.read(SourcePath.parse(path)));
                let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                SyntaxParser.parse_syntax_ruleset(&mut token_stream)
            )
        );
        let pattern = root_scope.StructuralFindAndReplace.compile_pattern(
            {
                .contents = args.pattern,
                .path = :Special "pattern",
            },
            .ruleset,
        );
        let replace = args.replace
            |> Option.map(
                contents => root_scope.StructuralFindAndReplace.compile_pattern(
                    {
                        .contents,
                        .path = :Special "replace pattern",
                    },
                    .ruleset = replace_ruleset,
                )
            );
        let process = (path :: SourcePath) => (
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );
            let founds = root_scope.StructuralFindAndReplace.find(&parsed.ast, &pattern);
            match replace with (
                | :None => (
                    for found in founds |> ArrayList.into_iter do (
                        root_scope.StructuralFindAndReplace.Found.print(&found);
                        (@current Output).write("\n");
                    );
                )
                | :Some replace_pattern => (
                    let mut output = Highlight.new_output(:Terminal);
                    let replace_ast = (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                        for found in &founds |> ArrayList.iter do (
                            # TODO this is hack only working currently
                            # I mean the equality - we are checking for identity equality
                            if found^.ast^ == ast^ then (
                                with Highlight.Context = {
                                    ...(@current Highlight.Context),
                                    .replace_ast = :Some (
                                        (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                                            for &{ .key = binding_name, .value = binding_ast } in &replace_pattern.bindings |> OrdMap.iter do (
                                                # TODO identity equality
                                                if ast^ == binding_ast^ then (
                                                    f((&found^.bindings |> OrdMap.get(binding_name) |> Option.unwrap)^);
                                                    break;
                                                );
                                            );
                                        )
                                    ),
                                };
                                f(&replace_pattern.ast);
                                break;
                            );
                        );
                    );
                    output.replace_ast = :Some replace_ast;
                    if args.inplace then (
                        let result = output_to_string(
                            () => (
                                Highlight.highlight(&parsed, output);
                                (@current Output).write("\n");
                            )
                        );
                        let path = match path |> SourcePath.file_path with (
                            | :Some path => path
                            | :None => Diagnostic.abort("Inplace is only available given file path")
                        );
                        # TODO std.fs.write_file
                        @native "(await import('fs')).writeFileSync(\(path), \(result))";
                    ) else (
                        Highlight.highlight(&parsed, output);
                    );
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
