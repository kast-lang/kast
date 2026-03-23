use (import "../deps/uri/src/lib.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;

module:

const Source = newtype {
    .contents :: String,
    .uri :: Uri,
};

impl Source as module = (
    module:

    const read_file = path -> Source => (
        let contents = std.fs.read_file(path);
        {
            .contents,
            .uri = Uri.new_path(path),
        }
    );

    const entire_span = (self :: &Source) -> Span => {
        .start = Position.beginning(),
        .end = (
            let mut position = Position.beginning();
            for c in String.iter(self^.contents) do (
                &mut position |> Position.advance(c);
            );
            position
        ),
        .uri = self^.uri,
    };
);