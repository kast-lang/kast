use (import "../deps/uri/src/lib.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;
use (import "./source_path.ks").*;
use std.rc.Rc;

module:

const Source = newtype {
    .contents :: Rc.t[String],
    .path :: SourcePath,
};

impl Source as module = (
    module:

    const read = (path :: &SourcePath) -> Source => (
        let contents = match path^ with (
            | :Stdin => std.io.stdin.read_to_end()
            | :Uri uri => if &uri^.scheme |> as_str == "file" then (
                std.fs.read_file(&uri^.path |> as_str)
            ) else (
                panic_fmt!("Unsupported uri scheme \(String.escape(&uri^.scheme |> as_str))")
            )
        );
        {
            .contents = Rc.new(contents),
            .path = path^,
        }
    );

    const entire_span = (self :: &Source) -> Span => {
        .start = Position.beginning(),
        .end = (
            let mut position = Position.beginning();
            for c in String.iter(&self^.contents^ |> as_str) do (
                &mut position |> Position.advance(c);
            );
            position
        ),
        .path = self^.path,
    };
);
