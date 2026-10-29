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

    const level_idx = (level :: Level) -> Int32 => match level with (
        | :Trace => 0
        | :Debug => 1
        | :Info => 2
        | :Warn => 3
        | :Error => 4
    );

    const min_level :: Level = :Info;

    const should_log = (level :: Level) -> Bool => (
        level_idx(level) >= level_idx(min_level)
    );

    const with_level_impl = (level :: Level, message :: () -> ()) => with_return (
        if not should_log(level) then return;
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
                let output = @current Stderr;
                with Output = output;
                output.write("[");
                output.write(level_text);
                output.write("] ");
                message();
                output.write("\n");
            ),
        );
    );

    const trace_fn = message => with_level_impl(:Trace, message);
    const debug_fn = message => with_level_impl(:Debug, message);
    const info_fn = message => with_level_impl(:Info, message);
    const warn_fn = message => with_level_impl(:Warn, message);
    const error_fn = message => with_level_impl(:Error, message);
    # Log.with_level!(:Trace, "message")
    const with_level = (args_ast :: std.Ast) -> std.Ast => (
        let args :: ArrayList.t[std.Ast] = args_ast
            |> std.Ast.get_comma_separated_list;
        if &args |> ArrayList.length != 2 then (
            panic("Expected exactly 2 args");
        );
        let level = args.[0];
        let format_string = args.[1];
        `(
            if should_log($level) then (
                with_level_impl(
                    $level,
                    () => (
                        # TODO still dont need to allocate a String
                        (@current Output).write(&format!($format_string) |> as_str);
                    ),
                );
            );
        )
    );

    const trace = (message :: std.Ast) -> std.Ast => with_level(`(:Trace, $message));
    const debug = (message :: std.Ast) -> std.Ast => with_level(`(:Debug, $message));
    const info = (message :: std.Ast) -> std.Ast => with_level(`(:Info, $message));
    const warn = (message :: std.Ast) -> std.Ast => with_level(`(:Warn, $message));
    const error = (message :: std.Ast) -> std.Ast => with_level(`(:Error, $message));
);
