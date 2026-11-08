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
use (import "../mini/_lib.ks").*;
use (import "../rc_str.ks").*;

const Cli = (
    module:

    use (import "./parse.ks").*;
    use (import "./highlight.ks").*;
    use (import "./format.ks").*;
    use (import "./structural_find_and_replace.ks").*;
    use (import "./generic_repl.ks").*;
);

const root_scope = @current_scope;

module:

const Mini = (
    module:

    const syntax_ruleset_path = String.from_str(root_scope.Mini.Compiler.ruleset_path());

    const Repl = (
        module:

        const Args = (
            module:

            const t = newtype {  };

            const parse = (
                start_index :: Int32,
            ) -> t => (
                let mut i = start_index;
                while i < std.sys.argc() do (
                    let arg = std.sys.argv_at(i);
                    Diagnostic.abort(&format!("Unexpected arg \(String.escape(arg))") |> as_str);
                    i += 1;
                );
                {  }
            );
        );

        const run = (common_args :: Common.Args.t, args :: Args.t) => (
            const Mini = root_scope.Mini;
            Cli.Repl.run_with(
                .ruleset = &Mini.Compiler.ruleset(),
                .eval = line => (
                    Ast.print(&line.parsed.ast);
                    locked_output.write_str("\n");
                )
            )
        );
    );

    const Compile = (
        module:

        const Args = (
            module:

            const t = newtype {
                .target :: Option.t[String],
                .prepend :: ArrayList.t[String],
                .paths :: ArrayList.t[String],
            };

            const parse = (
                start_index :: Int32,
            ) -> t => (
                let mut target = :None;
                let mut prepend = ArrayList.new();
                let mut paths = ArrayList.new();
                let mut i = start_index;
                while i < std.sys.argc() do (
                    let arg = std.sys.argv_at(i);
                    if arg == "--target" then (
                        target = :Some String.from_str(std.sys.argv_at(i + 1));
                        i += 2;
                        continue;
                    );
                    if arg == "--prepend" then (
                        ArrayList.push_back(&mut prepend, String.from_str(std.sys.argv_at(i + 1)));
                        i += 2;
                        continue;
                    );
                    &mut paths |> ArrayList.push_back(Common.path_arg(arg, .ext = :Some "mks"));
                    i += 1;
                );
                {
                    .target,
                    .prepend,
                    .paths,
                }
            );
        );

        const run = (common_args :: Common.Args.t, args :: Args.t) => (
            const Mini = root_scope.Mini;
            let target = &args.target
                |> Option.as_ref
                |> Option.unwrap_or_else(() => (
                    Diagnostic.abort("Specify compilation --target")
                ))
                |> as_str;
            let target :: Mini.Compiler.Target = if target == "c" then (
                :C
            ) else (
                Diagnostic.abort(&format!("Unknown target \(String.escape(target))") |> as_str)
            );
            if &args.paths |> ArrayList.length == 0 then (
                Diagnostic.abort("Expected at least 1 path");
            );
            let mut compiler = Mini.Compiler.init();
            for path in &args.paths |> ArrayList.iter do (
                let source = Source.read(&SourcePath.parse(path |> as_str));
                &mut compiler |> Mini.Compiler.add_source(source);
            );
            let program = compiler |> Mini.Compiler.compile;
            for path in &args.prepend |> ArrayList.iter do (
                println!("\(std.fs.read_file(path |> as_str))");
            );
            match target with (
                | :C => (
                    let compiled = Mini.Backends.C.compile(program);
                    Mini.Backends.C.print(&compiled);
                )
            );
        );
    );

    const Args = (
        module:

        const Subcommand = newtype (
            | :Parse Cli.Parse.Args.t
            | :Highlight Cli.Highlight.Args.t
            | :Format Cli.Format.Args.t
            | :StructuralFindAndReplace Cli.StructuralFindAndReplace.Args.t
            | :Repl Repl.Args.t
            | :Compile Compile.Args.t
        );

        const t = newtype {
            .subcommand :: Subcommand,
        };

        const parse = (start_index :: Int32) -> t => (
            let fix_syntax = :Some {
                .ruleset = syntax_ruleset_path,
                .ext = :Some String.from_str("mks"),
            };
            let mut common = Common.Args.default();
            let subcommand = unwindable subcommand (
                let mut i = start_index;
                while i < std.sys.argc() do (
                    let arg = std.sys.argv_at(i);
                    if arg == "parse" then (
                        unwind subcommand (:Parse Cli.Parse.Args.parse(i + 1, .fix_syntax));
                    );
                    if arg == "highlight" then (
                        unwind subcommand (:Highlight Cli.Highlight.Args.parse(i + 1, .fix_syntax));
                    );
                    if arg == "fmt" or arg == "format" then (
                        unwind subcommand (:Format Cli.Format.Args.parse(i + 1, .fix_syntax));
                    );
                    if arg == "find-ast" then (
                        unwind subcommand (:StructuralFindAndReplace Cli.StructuralFindAndReplace.Args.parse(i + 1, .fix_syntax));
                    );
                    if arg == "repl" then (
                        unwind subcommand (:Repl Repl.Args.parse(i + 1));
                    );
                    if arg == "compile" then (
                        unwind subcommand (:Compile Compile.Args.parse(i + 1));
                    );
                    break;
                    i += 1;
                );
                :Compile Compile.Args.parse(i)
            );
            {
                .subcommand,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        match args.subcommand with (
            | :Parse args => Cli.Parse.run(common_args, args)
            | :Highlight args => Cli.Highlight.run(common_args, args)
            | :Format args => Cli.Format.run(common_args, args)
            | :StructuralFindAndReplace args => Cli.StructuralFindAndReplace.run(common_args, args)
            | :Repl args => Repl.run(common_args, args)
            | :Compile args => Compile.run(common_args, args)
        );
    );
);
