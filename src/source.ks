use (import "../deps/uri/src/lib.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;
use (import "./source_path.ks").*;
use (import "./rc_str.ks").*;

module:

const Source = newtype {
    .contents :: RcStr,
    .path :: SourcePath,
};

impl Source as module = (
    module:

    const read = (path :: &SourcePath) -> Source => (
        let contents = match path^ with (
            | :Stdin => std.io.stdin.read_to_end()
            | :Uri uri => if &uri^.scheme |> String.as_str == "file" then (
                std.fs.read_file(&uri^.path |> String.as_str)
            ) else (
                panic_fmt!("Unsupported uri scheme \(String.escape(&uri^.scheme |> String.as_str))")
            )
        );
        {
            .contents = RcStr.from_String(contents),
            .path = path^,
        }
    );

    const entire_span = (self :: &Source) -> Span => {
        .start = Position.beginning(),
        .end = (
            let mut position = Position.beginning();
            for c in RcStr.iter(&self^.contents) do (
                &mut position |> Position.advance(c);
            );
            position
        ),
        .path = self^.path,
    };
);
