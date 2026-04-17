use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../syntax_ruleset.ks").*;

module:

const ParseSyntaxRuleset = (
    module:

    const Args = (
        module:

        const t = newtype {
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            for i in start_index..std.sys.argc() do (
                &mut paths |> ArrayList.push_back(std.sys.argv_at(i));
            );
            {
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let mut ruleset = SyntaxRuleset.new();
        let process = (path :: SourcePath) => (
            ansi.with_mode(
                :Bold,
                () => (@current Output).write("Parsing syntax rules from " + to_string(path) + "\n\n"),
            );
            let mut lexer = Lexer.new(Source.read(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            for &rule in ArrayList.iter(&rules) do (
                &mut ruleset |> SyntaxRuleset.add(rule);
            );
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
        SyntaxRuleset.print(&ruleset);
    )
)
