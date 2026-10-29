use (import "./common.ks").*;

module:

const FileState = newtype {
    .contents :: RcStr,
    .lines :: ArrayList.t[RcStr],
    .parsed :: Option.t[Parser.Parsed],
    .diagnostics :: ArrayList.t[Diagnostic.t],
};

const State = newtype {
    .@"syntax" :: {
        .kast :: SyntaxRuleset.t,
        .minikast :: SyntaxRuleset.t,
    },
    .files :: OrdMap.t[RcStr, FileState],
};

const get_ext = (path :: &str) -> Option.t[&str] => (
    let last_dot = path |> String.last_index_of('.');
    if last_dot < 0 then (
        :None
    ) else (
        :Some (path |> String.substring_from(last_dot + 1))
    )
);

const open_or_change_doc = (state :: &mut State, uri :: Uri, contents :: &RcStr) => (
    let contents = contents^;
    let uri_str = RcStr.from_String(Uri.to_string(&uri));
    Log.info!("open_or_change_doc \(uri_str)");
    let source :: Source = { .path = :Uri Rc.new(uri), .contents };
    let mut diagnostics = ArrayList.new();
    let parsed = (
        with Diagnostic.HandlerContext = {
            .stop_on_error = false,
            .handle = diagnostic => (
                &mut diagnostics |> ArrayList.push_back(Diagnostic.unstack(diagnostic));
            ),
        };
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let is_mks = match get_ext(&uri.path |> as_str) with (
            | :Some ext => ext == "mks"
            | :None => false
        );
        let ruleset = if is_mks then (
            &state^.@"syntax".minikast
        ) else (
            &state^.@"syntax".kast
        );
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        :Some parsed
    );
    let file_state = {
        .contents,
        .lines = (
            let mut lines = ArrayList.new();
            for line in RcStr.lines(&contents) do (
                &mut lines |> ArrayList.push_back(line);
            );
            lines
        ),
        .parsed,
        .diagnostics,
    };
    eprintln!("parsed \(uri_str)");
    &mut state^.files |> OrdMap.add(uri_str, file_state);
);

const did_open = (state :: &mut State, n :: Json.t) -> () => (
    let :Object ref fields = n;
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
        let (&(:String ref text)) = fields
            |> OrdMap.get(&RcStr.allocate("text"))
            |> Option.unwrap;
        { .uri = parse(uri |> RcStr.as_str), .text }
    );
    state |> open_or_change_doc(text_document.uri, text_document.text)
);

const did_change = (state :: &mut State, n :: Json.t) -> () => (
    let :Object ref fields = n;
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
    let (&(:Array ref changes)) = params
        |> OrdMap.get(&RcStr.allocate("contentChanges"))
        |> Option.unwrap;
    let (&(:Object ref change)) = changes |> ArrayList.at(0);
    let (&(:String ref text)) = change
        |> OrdMap.get(&RcStr.allocate("text"))
        |> Option.unwrap;
    state |> open_or_change_doc(text_document.uri, text)
);
