use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Diagnostic = (
    module:

    const AbortHandlerT = type (&str -> Never);
    const AbortHandler = @context AbortHandlerT;

    const default_abort_handler = (msg :: &str) -> Never => (
        with Output = (@current Stderr);
        lock_output(() => (
            ansi.with_mode(
                :Red,
                () => (
                    locked_output.write_str(msg);
                    locked_output.write_str("\n");
                )
            );
        ));
        std.sys.exit(-1)
    );
    ## Similar to panic but we don't care about stacktrace in this case
    ## Use for errors that are targeted at user
    const abort = [T] (msg :: &str) -> T => (
        (@current AbortHandler)(msg) |> from_never
    );

    const t = newtype {
        .severity :: Severity,
        .source :: Source,
        .span :: Span,
        .message :: () -> (),
        .related :: ArrayList.t[RelatedInfo]
    };

    const unstack_message = (message :: () -> ()) -> (() -> ()) => (
        let message = output_to_string(message);
        @move () => locked_output.write_str(&message |> as_str)
    );

    const unstack = (self :: Diagnostic.t) -> Diagnostic.t => {
        .severity = self.severity,
        .source = self.source,
        .span = self.span,
        .message = unstack_message(self.message),
        .related = (
            let mut list = ArrayList.new();
            for info in self.related |> ArrayList.into_iter do (
                let info = {
                    .span = info.span,
                    .message = unstack_message(info.message),
                };
                &mut list |> ArrayList.push_back(info);
            );
            list
        ),
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
        .unwind_on_error :: () -> Never,
    };

    const print_to_stderr = (diagnostic :: &Diagnostic.t) => (
        with Output = (@current Stderr);
        lock_output(() => (
            ansi.with_mode(
                :Red,
                () => (
                    let source_name :: Option.t[&str] = match diagnostic^.source with (
                        | :Internal => :Some "Internal"
                        | :Lexer => :Some "Lexer"
                        | :Parser => :Some "Parser"
                        | :Compiler => :Some "Compiler"
                        | :Interpreter => :Some "Interpreter"
                        | :Other => :None
                    );
                    match source_name with (
                        | :Some source_name => (
                            locked_output.write_str(source_name);
                            locked_output.write_str(" error at ");
                        )
                        | :None => (
                            locked_output.write_str("Error at ");
                        )
                    );
                    &diagnostic^.span |> Span.print;
                    locked_output.write_str(":\n");
                    diagnostic^.message();
                    for info in &diagnostic^.related |> ArrayList.iter do (
                        locked_output.write_str("\n");
                        ansi.with_mode(
                            :Dim,
                            () => (
                                locked_output.write_str("Note: at ");
                                Span.print(&info^.span);
                                locked_output.write_str("\n");
                            ),
                        );
                        info^.message();
                    );
                    locked_output.write_str("\n\n");
                ),
            );
        ));
    );

    const default_handler = (.stop_on_error :: Bool) -> Handler => {
        .stop_on_error,
        .handle = @move (diagnostic :: Diagnostic.t) => (
            print_to_stderr(&diagnostic);
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
        (@current UnwindableHandler).unwind_on_error() |> from_never
    );
);
