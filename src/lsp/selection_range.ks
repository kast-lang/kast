use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const selection_range = (state :: &State, request :: Json.t) -> Json.t => with_return (
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
    let file_state = get_latest_file_state(state, &text_document.uri)
        |> Option.unwrap_or_else(() => return :Null);
    let (&(:Array ref positions)) = params
        |> OrdMap.get(&RcStr.allocate("positions"))
        |> Option.unwrap;
    let mut result = ArrayList.new();
    for position in positions |> ArrayList.iter do (
        let position = position_from_lsp(position);
        &mut result |> ArrayList.push_back(selection_range_impl(&file_state^, position));
    );
    :Array result
);

const selection_range_impl = (
    file :: &ProcessedFile,
    position :: Position,
) -> Json.t => (
    let span = Span.single_char(
        .position,
        .char = ProcessedFile.char_at(file, position),
        .path = file^.source.path,
    );
    let mut result :: Json.t = :Object (
        let mut fields = OrdMap.new();
        &mut fields |> OrdMap.add(
            RcStr.allocate("range"),
            span_to_lsp_range(Source.entire_span(&file^.source)),
        );
        fields
    );
    if file^.parsed is :Some ref parsed then (
        for ast in iter_ast_to_span_inwards(&parsed^.ast, span) do (
            result = :Object (
                let mut fields = OrdMap.new();
                &mut fields |> OrdMap.add(
                    RcStr.allocate("range"),
                    span_to_lsp_range(ast^.span),
                );
                &mut fields |> OrdMap.add(
                    RcStr.allocate("parent"),
                    result,
                );
                fields
            );
        );
    );
    result
);
