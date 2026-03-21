use (import "./ansi.ks").*;
use (import "./output.ks").*;

module:

const Log = (
    module:
    
    const Level = newtype (
        | :Trace
        | :Debug
        | :Info
        | :Warn
        | :Error
    );
    
    const with_level = (level :: Level, message :: String) => (
        let { mode :: ansi.Mode, level_text } = match level with (
            | :Trace => { :Gray, "TRACE" }
            | :Debug => { :Cyan, "DEBUG" }
            | :Info => { :White, "INFO" }
            | :Warn => { :Yellow, "WARN" }
            | :Error => { :Red, "ERROR" }
        );
        ansi.with_mode(
            mode,
            () => (
                let output = @current Output;
                output.write("[");
                output.write(level_text);
                output.write("] ");
                output.write(message);
                output.write("\n");
            ),
        );
    );

    const trace = message => with_level(:Trace, message);
    const debug = message => with_level(:Debug, message);
    const info = message => with_level(:Info, message);
    const warn = message => with_level(:Warn, message);
    const error = message => with_level(:Error, message);
);
