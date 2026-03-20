use (import "./output.ks").*;
use (import "./uri.ks").*;
use (import "./position.ks").*;

module:

const Span = newtype {
    .start :: Position,
    .end :: Position,
    .uri :: Uri,
};

impl Span as module = (
    module:

    # <uri>:<start.line>.<start.column>-<end.line>.<end.column>
    const print = (self :: Span) => (
        let output = @current Output;
        self.uri |> Uri.print;
        output.write(":");
        self.start |> Position.print;
        output.write("-");
        self.end |> Position.print;
    );

    const single_char = (
        .position :: Position,
        .char :: Char,
        .uri :: Uri,
    ) -> Span => {
        .start = position,
        .end = Position.advance_copy(position, char),
        .uri,
    };
);