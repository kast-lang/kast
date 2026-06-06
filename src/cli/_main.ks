use (import "../diagnostic.ks").*;
use (import "../log.ks").*;
use (import "../output.ks").*;
use (import "../id.ks").*;
use (import "../lsp/_lib.ks").*;
use (import "./common.ks").*;
use (import "./format.ks").*;
use (import "./highlight.ks").*;
use (import "./parse_json.ks").*;
use (import "./parse_syntax_rules.ks").*;
use (import "./parse_syntax_ruleset.ks").*;
use (import "./parse.ks").*;
use (import "./repl.ks").*;
use (import "./run.ks").*;
use (import "./structural_find_and_replace.ks").*;
use (import "./tokenize.ks").*;
use (import "./mini.ks").*;

with IdGenCtx = IdGen.new();

with Stdout = new_std_output(
    std.io.stdout.write,
    .color = Common.output_color(std.io.stdout.isatty(), :Auto),
);
with Stderr = new_std_output(
    std.io.stderr.write,
    .color = Common.output_color(std.io.stderr.isatty(), :Auto),
);
with Output = (@current Stdout);
with Diagnostic.AbortHandler = Diagnostic.default_abort_handler;

const Args = (
    module:

    const Subcommand = newtype (
        | :Tokenize Tokenize.Args.t
        | :ParseSyntaxRules ParseSyntaxRules.Args.t
        | :ParseSyntaxRuleset ParseSyntaxRuleset.Args.t
        | :Parse Parse.Args.t
        | :ParseJson ParseJson.Args.t
        | :Highlight Highlight.Args.t
        | :Format Format.Args.t
        | :Lsp Lsp.CliArgs.t
        | :StructuralFindAndReplace StructuralFindAndReplace.Args.t
        | :Repl Repl.Args.t
        | :Run Run.Args.t
        | :Mini Mini.Args.t
    );

    const t = newtype {
        .subcommand :: Subcommand,
        .common :: Common.Args.t,
    };

    const parse = () -> t => (
        let mut common = Common.Args.default();
        let subcommand = unwindable subcommand (
            let mut i = 1;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "lex" or arg == "tokenize" then (
                    unwind subcommand (:Tokenize Tokenize.Args.parse(i + 1));
                );
                if arg == "parse-syntax-rules" then (
                    unwind subcommand (:ParseSyntaxRules ParseSyntaxRules.Args.parse(i + 1));
                );
                if arg == "parse-syntax-ruleset" then (
                    unwind subcommand (:ParseSyntaxRuleset ParseSyntaxRuleset.Args.parse(i + 1));
                );
                if arg == "parse" then (
                    unwind subcommand (:Parse Parse.Args.parse(i + 1, .fix_syntax = :None));
                );
                if arg == "parse-json" then (
                    unwind subcommand (:ParseJson ParseJson.Args.parse(i + 1));
                );
                if arg == "highlight" then (
                    unwind subcommand (:Highlight Highlight.Args.parse(i + 1, .fix_syntax = :None));
                );
                if arg == "fmt" or arg == "format" then (
                    unwind subcommand (:Format Format.Args.parse(i + 1, .fix_syntax = :None));
                );
                if arg == "lsp" then (
                    unwind subcommand (:Lsp Lsp.CliArgs.parse(i + 1));
                );
                if arg == "find-ast" then (
                    unwind subcommand (:StructuralFindAndReplace StructuralFindAndReplace.Args.parse(i + 1, .fix_syntax = :None));
                );
                if arg == "repl" then (
                    unwind subcommand (:Repl Repl.Args.parse(i + 1));
                );
                if arg == "run" then (
                    unwind subcommand (:Run Run.Args.parse(i + 1));
                );
                if arg == "mini" then (
                    unwind subcommand (:Mini Mini.Args.parse(i + 1));
                );
                &mut common |> Common.Args.parse_arg(&mut i);
            );
            :Repl Repl.Args.default()
        );
        {
            .subcommand,
            .common,
        }
    );
);

let { .common = common_args, .subcommand } = Args.parse();

with Stdout = new_std_output(
    std.io.stdout.write,
    .color = Common.output_color(std.io.stdout.isatty(), common_args.color),
);
with Stderr = new_std_output(
    std.io.stderr.write,
    .color = Common.output_color(std.io.stderr.isatty(), common_args.color),
);
with Output = (@current Stdout);

with Diagnostic.HandlerContext = Diagnostic.default_handler(.stop_on_error = common_args.stop_on_error);
match subcommand with (
    | :Tokenize args => Tokenize.run(common_args, args)
    | :ParseSyntaxRules args => ParseSyntaxRules.run(common_args, args)
    | :ParseSyntaxRuleset args => ParseSyntaxRuleset.run(common_args, args)
    | :Parse args => Parse.run(common_args, args)
    | :ParseJson args => ParseJson.run(common_args, args)
    | :Highlight args => Highlight.run(common_args, args)
    | :Format args => Format.run(common_args, args)
    | :Lsp args => Lsp.run(args)
    | :StructuralFindAndReplace args => StructuralFindAndReplace.run(common_args, args)
    | :Repl args => Repl.run(common_args, args)
    | :Run args => Run.run(common_args, args)
    | :Mini args => Mini.run(common_args, args)
);
