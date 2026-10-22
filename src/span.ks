use (import "./output.ks").*;
use (import "./position.ks").*;
use (import "./source_path.ks").*;

module:

const Span = newtype {
    .start :: Position,
    ## end is exclusive
    .end :: Position,
    .path :: SourcePath,
};

impl Span as module = (
    module:

    # <source_path>:<start.line>.<start.column>-<end.line>.<end.column>
    const print = (self :: &Span) => (
        let output = @current Output;
        &self^.path |> SourcePath.print;
        output.write(":");
        &self^.start |> Position.print;
        output.write("-");
        &self^.end |> Position.print;
    );

    const empty = (
        .position :: Position,
        .path :: SourcePath,
    ) -> Span => {
        .start = position,
        .end = position,
        .path,
    };

    const single_char = (
        .position :: Position,
        .char :: Option.t[Char],
        .path :: SourcePath,
    ) -> Span => {
        .start = position,
        .end = match char with (
            | :Some char => Position.advance_copy(position, char)
            | :None => position
        ),
        .path,
    };
);
