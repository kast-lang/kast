use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../position.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
use (import "../structural_find_and_replace.ks").*;
use (import "../rc_str.ks").*;
use (import "../stdext/_lib.ks").*;
use std.collections.OrdMap;
use std.rc.Rc;

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
                        .ruleset = String.from_str(std.sys.argv_at(i + 1)),
                        .ext = :None,
                    };
                    i += 2;
                    continue;
                );
                if arg == "--pattern" then (
                    pattern = :Some String.from_str(std.sys.argv_at(i + 1));
                    i += 2;
                    continue;
                );
                if arg == "--replace" then (
                    replace = :Some String.from_str(std.sys.argv_at(i + 1));
                    i += 2;
                    continue;
                );
                if arg == "--replace-ruleset" then (
                    replace_ruleset = :Some String.from_str(std.sys.argv_at(i + 1));
                    i += 2;
                    continue;
                );
                if arg == "--inplace" then (
                    inplace = true;
                    i += 1;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg_for_syntax(
                    arg,
                    .@"syntax" = &@"syntax" |> Option.as_ref,
                ));
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
        let ruleset_path = match args.ruleset with (
            | :Some ref path => path |> as_str
            | :None => "kast:///std/syntax.ks"
        );
        let mut lexer = Lexer.new(Source.read(&SourcePath.parse(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let replace_ruleset = match args.replace_ruleset with (
            | :None => SyntaxRuleset.clone(&ruleset)
            | :Some ref path => (
                let path = path |> as_str;
                let mut lexer = Lexer.new(Source.read(&SourcePath.parse(path)));
                let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                SyntaxParser.parse_syntax_ruleset(&mut token_stream)
            )
        );
        let pattern = root_scope.StructuralFindAndReplace.compile_pattern(
            {
                .contents = RcStr.allocate(&args.pattern |> as_str),
                .path = :Special RcStr.allocate("pattern"),
            },
            .ruleset = &ruleset,
        );
        let replace = &args.replace
            |> Option.as_ref
            |> Option.map(contents => root_scope.StructuralFindAndReplace.compile_pattern(
                {
                    .contents = RcStr.allocate(contents |> as_str),
                    .path = :Special RcStr.allocate("replace pattern"),
                },
                .ruleset = &replace_ruleset,
            ));
        let process = (path :: &SourcePath) => (
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset = &ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );
            let founds = root_scope.StructuralFindAndReplace.find(&parsed.ast, &pattern);
            match replace with (
                | :None => (
                    lock_output(() => (
                        for found in founds |> ArrayList.into_iter do (
                            root_scope.StructuralFindAndReplace.Found.print(&found);
                            locked_output.write_str("\n");
                        );
                    ));
                )
                | :Some replace_pattern => (
                    let mut output = Highlight.new_output(:Terminal);
                    let replace_ast = (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                        for found in &founds |> ArrayList.iter do (
                            # we are checking for identity equality
                            if @native "\(found^.ast) == \(ast)" then (
                                with Highlight.Context = {
                                    ...(@current Highlight.Context),
                                    .replace_ast = :Some (
                                        (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                                            for &{ .key = binding_name, .value = binding_ast } in &replace_pattern.bindings |> OrdMap.iter do (
                                                if @native "\(ast) == \(binding_ast)" then (
                                                    f((&found^.bindings |> OrdMap.get(&binding_name) |> Option.unwrap)^);
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
                        let result = output_to_string(() => (
                            Highlight.highlight(&parsed, output);
                            locked_output.write_str("\n");
                        ));
                        let path = match path |> SourcePath.file_path with (
                            | :Some path => path
                            | :None => Diagnostic.abort("Inplace is only available given file path")
                        );
                        stdext.fs.write(path, &result |> as_str);
                    ) else (
                        lock_output(() => (
                            Highlight.highlight(&parsed, output);
                        ));
                    );
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
