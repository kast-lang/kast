use (import "./common.ks").*;

module:

const FileState = newtype {
    .contents :: String,
    .lines :: ArrayList.t[String],
    .parsed :: Option.t[Parser.Parsed],
    .diagnostics :: ArrayList.t[Diagnostic.t],
};

const State = newtype {
    .@"syntax" :: {
        .kast :: SyntaxRuleset.t,
        .minikast :: SyntaxRuleset.t,
    },
    .files :: OrdMap.t[String, FileState],
};

const get_ext = (path :: String) -> Option.t[String] => (
    let last_dot = path |> String.last_index_of('.');
    if last_dot < 0 then (
        :None
    ) else (
        :Some (path |> String.substring_from(last_dot + 1))
    )
);

const open_or_change_doc = (state :: &mut State, uri :: Uri, contents :: String) => (
    Log.info_msg("open_or_change_doc " + Uri.to_string(uri));
    let source :: Source = { .path = :Uri uri, .contents };
    let entire_source_span = (
        let start = Position.beginning();
        let mut end = start;
        for c in source.contents |> String.iter do (
            &mut end |> Position.advance(c);
        );
        {
            .start,
            .end,
            .path = source.path,
        }
    );
    let mut diagnostics = ArrayList.new();
    let parsed = (
        with Diagnostic.HandlerContext = {
            .stop_on_error = false,
            .handle = diagnostic => (
                &mut diagnostics |> ArrayList.push_back(diagnostic);
            ),
        };
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = if std.repr.structurally_equal(get_ext(uri.path), :Some "mks") then (
            state^.@"syntax".minikast
        ) else (
            state^.@"syntax".kast
        );
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span,
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        :Some parsed
    );
    let file_state = {
        .contents,
        .lines = (
            let mut lines = ArrayList.new();
            for line in String.lines(contents) do (
                &mut lines |> ArrayList.push_back(line);
            );
            lines
        ),
        .parsed,
        .diagnostics,
    };
    &mut state^.files |> OrdMap.add(Uri.to_string(uri), file_state);
);

const did_open = (state :: &mut State, n :: Json.t) -> () => (
    let :Object fields = n;
    let (&(:Object params)) = &fields |> OrdMap.get("params") |> Option.unwrap;
    let text_document = (
        let (&value) = &params |> OrdMap.get("textDocument") |> Option.unwrap;
        let :Object fields = value;
        let (&(:String uri)) = &fields |> OrdMap.get("uri") |> Option.unwrap;
        let (&(:String text)) = &fields |> OrdMap.get("text") |> Option.unwrap;
        { .uri = parse(uri), .text }
    );
    state |> open_or_change_doc(text_document.uri, text_document.text)
);

const did_change = (state :: &mut State, n :: Json.t) -> () => (
    let :Object fields = n;
    let (&(:Object params)) = &fields |> OrdMap.get("params") |> Option.unwrap;
    let text_document = (
        let (&value) = &params |> OrdMap.get("textDocument") |> Option.unwrap;
        let :Object fields = value;
        let (&(:String uri)) = &fields |> OrdMap.get("uri") |> Option.unwrap;
        { .uri = parse(uri) }
    );
    let (&(:Array changes)) = &params |> OrdMap.get("contentChanges") |> Option.unwrap;
    let (&(:Object change)) = &changes |> ArrayList.at(0);
    let (&(:String text)) = &change |> OrdMap.get("text") |> Option.unwrap;
    state |> open_or_change_doc(text_document.uri, text)
);
