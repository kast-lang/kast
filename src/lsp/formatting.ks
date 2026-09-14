use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const formatting = (
    module:

    const format = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        let :Object fields = request;
        let (&(:Object params)) = &fields |> OrdMap.get("params") |> Option.unwrap;
        let text_document = (
            let (&value) = &params |> OrdMap.get("textDocument") |> Option.unwrap;
            let :Object fields = value;
            let (&(:String uri)) = &fields |> OrdMap.get("uri") |> Option.unwrap;
            { .uri = parse(uri) }
        );

        let file_state = &state^.files
            |> OrdMap.get(Uri.to_string(text_document.uri))
            |> Option.unwrap_or_else(() => return :Null);
        let parsed = &file_state^.parsed
            |> Option.as_ref
            |> Option.unwrap_or_else(() => return :Null);

        let new_text = Format.format_to_string(parsed);

        let edit = :Object (
            let mut fields = OrdMap.new();
            &mut fields
                |> OrdMap.add(
                    "range",
                    span_to_lsp_range(
                        {
                            .start = Position.beginning(),
                            .end = parsed^.eof,
                            .path = parsed^.ast.span.path,
                        }
                    )
                );
            &mut fields |> OrdMap.add("newText", :String new_text);
            fields
        );

        :Array (
            let mut edits = ArrayList.new();
            &mut edits |> ArrayList.push_back(edit);
            edits
        )
    );
);
