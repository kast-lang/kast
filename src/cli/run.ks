use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
use (import "../diagnostic.ks").*;
use (import "../readline.ks").*;

use (import "../hir/_lib.ks").*;
use (import "../compiler/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;

module:

const Run = (
    module:

    const Args = (
        module:

        const t = newtype {
            .path :: Option.t[String],
        };

        const parse = (
            start_index :: Int32,
        ) -> t => (
            let mut path = :None;
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if path is :Some _ then (
                    Diagnostic.abort("Expected a single path argument");
                );
                path = :Some Common.path_arg_for_syntax(arg, .@"syntax" = :None);
                i += 1;
            );
            {
                .path,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset = Common.default_syntax_ruleset();
        let path = match args.path with (
            | :Some path => SourcePath.parse(path)
            | :None => :Stdin
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
        Common.with_compiler_interpreter(
            () => (
                let expr = Compiler.compile[Expr](&parsed.ast, .expected_ty = :None);
                Interpreter.eval(&expr);
            ),
        );
    );
);
