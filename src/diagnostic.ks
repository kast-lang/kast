use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Diagnostic = (
    module:

    const AbortHandlerT = type ([T] String -> T);
    const AbortHandler = @context AbortHandlerT;

    const default_abort_handler = [T] (msg :: String) -> T => (
        with Output = (@current Stderr);
        ansi.with_mode(
            :Red,
            () => (
                let output = @current Output;
                output.write(msg);
                output.write("\n");
            )
        );
        std.sys.exit(-1)
    );
    ## Similar to panic but we don't care about stacktrace in this case
    ## Use for errors that are targeted at user
    const abort = [T] (msg :: String) -> T => (
        (@current AbortHandler)(msg)
    );

    const t = newtype {
        .severity :: Severity,
        .source :: Source,
        .span :: Span,
        .message :: () -> (),
        .related :: ArrayList.t[RelatedInfo]
    };

    const RelatedInfo = newtype {
        .span :: Span,
        .message :: () -> (),
    };

    const Severity = newtype (
        | :Error
        | :Warning
        | :Info
        | :Hint
    );

    const Source = newtype (
        | :Lexer
        | :Parser
        | :Compiler
        | :Interpreter
        ## Internal error is a bug in the implementation of kast
        | :Internal
        | :Other
    );

    const Handler = newtype {
        .stop_on_error :: Bool,
        .handle :: Diagnostic.t -> (),
    };

    const HandlerContext = @context Handler;

    const UnwindableHandler = @context newtype {
        .unwind_on_error :: [T] () -> T,
    };

    const default_handler = (.stop_on_error :: Bool) -> Handler => {
        .stop_on_error,
        .handle = (diagnostic :: Diagnostic.t) => (
            with Output = (@current Stderr);
            let output = @current Output;
            ansi.with_mode(
                :Red,
                () => (
                    let source_name = match diagnostic.source with (
                        | :Internal => :Some "Internal"
                        | :Lexer => :Some "Lexer"
                        | :Parser => :Some "Parser"
                        | :Compiler => :Some "Compiler"
                        | :Other => :None
                    );
                    match source_name with (
                        | :Some source_name => (
                            output.write(source_name);
                            output.write(" error at ");
                        )
                        | :None => (
                            output.write("Error at ");
                        )
                    );
                    diagnostic.span |> Span.print;
                    output.write(":\n");
                    diagnostic.message();
                    for info in diagnostic.related |> ArrayList.into_iter do (
                        output.write("\n");
                        ansi.with_mode(
                            :Dim,
                            () => (
                                output.write("Note: at ");
                                Span.print(info.span);
                                output.write("\n");
                            ),
                        );
                        info.message();
                    );
                    output.write("\n\n");
                ),
            );
            if stop_on_error then (
                std.sys.exit(-1);
            );
        ),
    };

    const report = (diagnostic :: Diagnostic.t) => (
        (@current HandlerContext).handle(diagnostic);
    );

    const report_and_unwind = [T] (diagnostic :: Diagnostic.t) -> T => (
        (@current HandlerContext).handle(diagnostic);
        (@current UnwindableHandler).unwind_on_error()
    );
);
