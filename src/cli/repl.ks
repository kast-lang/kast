use (import "./generic_repl.ks").*;

use (import "../hir/_lib.ks").*;
use (import "../compiler/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;

const super = @current_scope;

module:

const Repl = (
    module:

    use super.Repl.*;

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset = Common.default_syntax_ruleset();
        let eval = (line :: Line) => with_return (
            with Diagnostic.UnwindableHandler = {
                .unwind_on_error = () -> Never => (
                    return
                ),
            };
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    Diagnostic.print(diagnostic);
                ),
            };
            let expr = Compiler.compile[Expr](&line.parsed.ast, .expected_ty = :None);
            let value = Interpreter.eval(&expr);
            if value.shape is :Unit then () else (
                Value.print(&value);
                (@current Output).write("\n");
            );
        );
        Common.with_compiler_interpreter(
            () => run_with(.ruleset, .eval),
        );
    );
);
