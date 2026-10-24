use (import "./src/output.ks").*;
use (import "./src/diagnostic.ks").*;
use (import "./src/span.ks").*;
use (import "./src/position.ks").*;
use (import "./src/rc_str.ks").*;

with Stdout = new_std_output(
    std.io.stdout.write,
    .color = true,
);
with Stderr = new_std_output(
    std.io.stderr.write,
    .color = true,
);
with Output = (@current Stdout);
with Diagnostic.HandlerContext = Diagnostic.default_handler(.stop_on_error = true);

let diagnostic = {
    .severity = :Error,
    .source = :Compiler,
    .span = {
        .start = Position.beginning(),
        .end = Position.beginning(),
        .path = :Special RcStr.from_String(String.from_str("test.ks")),
    },
    .message = () => (),
    .related = ArrayList.new(),
};
Diagnostic.report_and_unwind(diagnostic);

println!("HI");

