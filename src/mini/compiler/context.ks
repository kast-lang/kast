use (import "./common.ks").*;
use (import "./template.ks").*;

module:

const parse_context_type = (ast :: Ast.t) -> ContextType => (
    eval_ast(:Some { .shape = :ContextType, .alias_name = :None }, ast)
        |> Value.expect_context_type(.span = ast.span)
);
