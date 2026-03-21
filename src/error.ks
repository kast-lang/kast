use (import "./output.ks").*;
use (import "./ansi.ks").*;
use (import "./span.ks").*;

module:

const Error = (
    module:
    
    const Handler = newtype {
        .stop_on_error :: Bool,
        .handle :: (Span, String) -> (),
    };
    
    const HandlerContext = @context Handler;

    const UnwindableHandler = @context newtype {
        .unwind_on_error :: [T] () -> T,
    };
    
    const init_handler = (.stop_on_error :: Bool) -> Handler => {
        .stop_on_error,
        .handle = (span, message) => (
            let output = @current Output;
            ansi.with_mode(
                :Red,
                () => (
                    output.write("ERROR at ");
                    span |> Span.print;
                    output.write(":\n");
                    output.write(message);
                    output.write("\n\n");
                ),
            );
            if stop_on_error then (
                std.sys.exit(-1);
            );
        )
    };
    
    const report = (span :: Span, message :: String) => (
        (@current HandlerContext).handle(span, message);
    );
    
    const report_and_unwind = [T] (span :: Span, message :: () -> ()) -> T => (
        let output = @current Output;
        ansi.with_mode(
            :Red,
            () => (
                output.write("ERROR at ");
                span |> Span.print;
                output.write(":\n");
                message();
                output.write("\n\n");
            ),
        );
        if (@current HandlerContext).stop_on_error then (
            std.sys.exit(-1);
        );
        (@current UnwindableHandler).unwind_on_error()
    );
);
