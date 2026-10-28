use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const hover = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
    let :Object ref fields = request;
    let (&(:Object ref params)) = fields
        |> OrdMap.get(&RcStr.allocate("params"))
        |> Option.unwrap;
    let text_document = (
        let value = params
            |> OrdMap.get(&RcStr.allocate("textDocument"))
            |> Option.unwrap;
        let :Object ref fields = value^;
        let (&(:String ref uri)) = fields
            |> OrdMap.get(&RcStr.allocate("uri"))
            |> Option.unwrap;
        { .uri = parse(uri |> RcStr.as_str) }
    );
    let position :: Position = (
        let value = params
            |> OrdMap.get(&RcStr.allocate("position"))
            |> Option.unwrap;
        let :Object ref fields = value^;
        let (&(:Number line)) = fields
            |> OrdMap.get(&RcStr.allocate("line"))
            |> Option.unwrap;
        let (&(:Number column)) = fields
            |> OrdMap.get(&RcStr.allocate("character"))
            |> Option.unwrap;
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

    let uri_str = RcStr.from_String(Uri.to_string(&text_document.uri));
    eprintln!("Hovering \(uri_str)");
    let file_state = &state^.files
        |> OrdMap.get(&uri_str)
        |> Option.unwrap_or_else(() => return :Null);
    let hovered_char :: Option.t[_] = with_return (
        if (
            position.line < 0
            or position.line >= ArrayList.length(&file_state^.lines)
        ) then (
            return :None;
        );
        let line_contents = &file_state^.lines |> ArrayList.at(position.line);
        if position.column.string_encoding >= RcStr.length(line_contents) then (
            return :None;
        );
        :Some RcStr.at(line_contents, position.column.string_encoding)
    );
    let hover_text = match hovered_char with (
        | :Some c => (
            format!(''
                Hovered char: `\(c)`

                position: \(position.line + 1):\(position.column.string_encoding + 1)'')
        )
        | :None => format!("Hovering out of range?")
    );
    :Object (
        let mut fields = OrdMap.new();
        &mut fields
            |> OrdMap.add(
                RcStr.allocate("contents"),
                :String RcStr.from_String(hover_text),
            );
        fields
    )
);
