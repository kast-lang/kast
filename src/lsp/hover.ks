use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const hover = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
    let :Object fields = request;
    let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
    let text_document = (
        let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
        let :Object fields = value;
        let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
        { .uri = parse(uri) }
    );
    let position :: Position = (
        let &value = &params |> OrdMap.get("position") |> Option.unwrap;
        let :Object fields = value;
        let &(:Number line) = &fields |> OrdMap.get("line") |> Option.unwrap;
        let &(:Number column) = &fields |> OrdMap.get("character") |> Option.unwrap;
        let line = std.convert.float64_to_int32(line);
        let column = std.convert.float64_to_int32(column);
        {
            .string_encoding_index = 0,
            .line,
            .column = {
                .string_encoding = column,
                .display_width = 0,
            },
        }
    );

    let file_state = &state^.files
        |> OrdMap.get(Uri.to_string(text_document.uri))
        |> Option.unwrap_or_else(() => return :Null);
    eprint(file_state^.contents);
    let hovered_char :: Option.t[_] = with_return (
        if (
            position.line < 0
            or position.line >= ArrayList.length(&file_state^.lines)
        ) then (
            return :None;
        );
        let line_contents = &file_state^.lines |> ArrayList.at(position.line);
        if position.column.string_encoding >= String.length(line_contents^) then (
            return :None;
        );
        :Some String.at(line_contents^, position.column.string_encoding)
    );
    let hover_text = match hovered_char with (
        | :Some c => (
            "Hovered char: `"
            + to_string(c)
            + "`\n\nposition: "
            + to_string(position.line + 1)
            + ":"
            + to_string(position.column.string_encoding + 1)
        )
        | :None => "Hovering out of range?"
    );
    :Object (
        let mut fields = OrdMap.new();
        &mut fields |> OrdMap.add("contents", :String hover_text);
        fields
    )
);
