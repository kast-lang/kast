use (import "./common.ks").*;
use (import "./state.ks").*;
use std.StringBuilder;

module:

const LocationLink = newtype {
    .origin :: Span,
    .target :: Span,
    .target_selection :: Span,
};

impl LocationLink as module = (
    module:
    const to_lsp = (self :: LocationLink) -> Json.t => (
        :Object (
            let mut fields = OrdMap.new();
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("originSelectionRange"),
                    span_to_lsp_range(self.origin),
                );
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("targetUri"),
                    path_to_uri_lsp(self.target.path),
                );
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("targetRange"),
                    span_to_lsp_range(self.target),
                );
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("targetSelectionRange"),
                    span_to_lsp_range(self.target_selection),
                );
            fields
        )
    );
);

const definition = (state :: &State, request :: Json.t) -> Json.t => with_return (
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
    let position :: Position = position_from_lsp(
        params
            |> OrdMap.get(&RcStr.allocate("position"))
            |> Option.unwrap
    );
    let file_state = get_latest_file_state(state, &text_document.uri)
        |> Option.unwrap_or_else(() => return :Null);

    let hover = hover_impl(&file_state^, position)
        |> Option.unwrap_or_else(() => return :Null);
    let ast = hover.ast
        |> Option.unwrap_or_else(() => return :Null);
    let link :: LocationLink = match ast^.shape^ with (
        | :Rule { .rule = ref rule, ... } => {
            .origin = ast^.span,
            .target = rule^^.span,
            .target_selection = rule^^.span,
        }
        | _ => return :Null
    );
    LocationLink.to_lsp(link)
);

const hover = (state :: &State, request :: Json.t) -> Json.t => with_return (
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
    let position :: Position = position_from_lsp(
        params
            |> OrdMap.get(&RcStr.allocate("position"))
            |> Option.unwrap
    );

    let uri_str = RcStr.from_String(Uri.to_string(&text_document.uri));
    eprintln!("Hovering \(uri_str)");
    let file_state = get_latest_file_state(state, &text_document.uri)
        |> Option.unwrap_or_else(() => return :Null);
    let hover = hover_impl(&file_state^, position)
        |> Option.unwrap_or_else(() => return :Null);
    :Object (
        let mut fields = OrdMap.new();
        &mut fields
            |> OrdMap.add(
                RcStr.allocate("contents"),
                :String RcStr.from_String(hover.text),
            );
        if hover.ast is :Some ast then (
            &mut fields
                |> OrdMap.add(
                    RcStr.allocate("range"),
                    span_to_lsp_range(ast^.span),
                );
        );
        fields
    )
);

const Hover = newtype {
    .text :: String,
    .ast :: Option.t[type (&Ast.t)],
};

const hover_impl = (file :: &ProcessedFile, position :: Position) -> Option.t[Hover] => (
    let mut hovered_ast = :None;
    let mut sections = ArrayList.new[String]();
    let hovered_char :: Option.t[Char] = with_return (
        if (
            position.line < 0
            or position.line >= ArrayList.length(&file^.lines)
        ) then (
            return :None;
        );
        let line_contents = &file^.lines |> ArrayList.at(position.line);
        if position.column.string_encoding >= RcStr.length(line_contents) then (
            return :None;
        );
        :Some RcStr.at(line_contents, position.column.string_encoding)
    );
    let span = Span.single_char(
        .position,
        .char = hovered_char,
        .path = file^.source.path,
    );
    if file^.parsed is :Some ref parsed then (
        let hovered_ast_node :: Option.t[type (&Ast.t)] = with_return (
            for ast in iter_ast_from_span_outwards(&parsed^.ast, span) do (
                return :Some ast;
            );
            :None
        );
        if hovered_ast_node is :Some ast then (
            let text = match ast^.shape^ with (
                | :Empty => format!("<empty>")
                | :Token _ => format!("<token>")
                | :InterpolatedString _ => (
                    format!("<interpolated string>")
                )
                | :Rule {
                    .rule = ref rule,
                    .root = _,
                } => (
                    format!("syntax rule: \(rule^^.name)")
                )
                | :Syntax {
                    .command = ref command,
                    .value_after = _,
                } => (
                    format!("<syntax extension>")
                )
                | :Error _ => (
                    format!("<error>")
                )
            );
            &mut sections |> ArrayList.push_back(text);
            hovered_ast = :Some ast;
        );
    );
    (
        let text = match hovered_char with (
            | :Some c => (
                format!(''
                    Hovered char: `\(c)`
                    
                    position: \(position.line + 1):\(position.column.string_encoding + 1)'')
            )
            | :None => format!("Hovering out of range?")
        );
        &mut sections |> ArrayList.push_back(text);
    );
    let mut result = StringBuilder.new();
    for { i, section } in sections |> ArrayList.into_iter |> std.iter.enumerate do (
        if i != 0 then (
            &mut result |> StringBuilder.add_str("\n\n");
        );
        &mut result |> StringBuilder.add_String(section);
    );
    :Some {
        .text = result |> StringBuilder.into_string,
        .ast = hovered_ast,
    }
);
