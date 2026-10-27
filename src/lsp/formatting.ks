use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const formatting = (
    module:

    const format = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
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

        let file_state = &state^.files
            |> OrdMap.get(&RcStr.from_String(Uri.to_string(&text_document.uri)))
            |> Option.unwrap_or_else(() => return :Null);
        let parsed = &file_state^.parsed
            |> Option.as_ref
            |> Option.unwrap_or_else(() => return :Null);

        let new_text = Format.format_to_string(parsed);

        let edit = :Object (
            let mut fields = OrdMap.new();
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("range"),
                    span_to_lsp_range(
                        {
                            .start = Position.beginning(),
                            .end = parsed^.eof,
                            .path = parsed^.ast.span.path,
                        }
                    )
                );
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("newText"),
                    :String RcStr.from_String(new_text),
                );
            fields
        );

        :Array (
            let mut edits = ArrayList.new();
            &mut edits |> ArrayList.push_back(edit);
            edits
        )
    );
);
