use (import "./common.ks").*;

module:

const FileVersion = Int32;

const FileState = newtype {
    .processed :: Mutex.t[Option.t[Rc.t[ProcessedFile]]],
    .latest_version :: Mutex.t[FileVersion],
    .path :: SourcePath,
};

const ProcessedFile = newtype {
    .source :: Source,
    .lines :: ArrayList.t[RcStr],
    .parsed :: Option.t[Parser.Parsed],
    .diagnostics :: ArrayList.t[Diagnostic.t],
    .version :: FileVersion,
};

impl ProcessedFile as module = (
    module:

    const char_at = (
        file :: &ProcessedFile,
        position :: Position,
    ) -> Option.t[Char] => with_return (
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
);

const State = newtype {
    .@"syntax" :: {
        .kast :: SyntaxRuleset.t,
        .minikast :: SyntaxRuleset.t,
    },
    .files :: Mutex.t[OrdMap.t[RcStr, Rc.t[FileState]]],
};

const get_ext = (path :: &str) -> Option.t[&str] => (
    let last_dot = path |> String.last_index_of('.');
    if last_dot < 0 then (
        :None
    ) else (
        :Some (path |> String.substring_from(last_dot + 1))
    )
);

const get_latest_file_state = (state :: &State, uri :: &Uri) -> Option.t[Rc.t[ProcessedFile]] => with_return (
    let file_state :: Rc.t[FileState] = (
        let files = Mutex.lock(&state^.files);
        @defer Mutex.unlock(files);
        let file_state = &files.value^
            |> OrdMap.get(&RcStr.from_String(Uri.to_string(uri)))
            |> Option.unwrap_or_else(() => return :None);
        file_state^
    );
    @loop ( unwindable l (
        let processed :: Rc.t[ProcessedFile] = (
            let file_state = Mutex.lock(&file_state^.processed);
            @defer Mutex.unlock(file_state);
            match file_state.value^ with (
                | :Some file_state => file_state
                | :None => unwind l ()
            )
        );
        let latest_version = Mutex.lock(&file_state^.latest_version);
        @defer Mutex.unlock(latest_version);
        if processed^.version == latest_version.value^ then (
            return :Some processed;
        ) else (
            return :None;
        );
    ) )
);

const open_or_change_doc = (state :: &State, uri :: Uri, contents :: &RcStr) => (
    let uri = Rc.new(uri);
    let uri_str = RcStr.from_String(Uri.to_string(&uri^));
    Log.info!("open_or_change_doc \(uri_str)");
    let { file :: Rc.t[FileState], my_version } = (
        let files = Mutex.lock(&state^.files);
        @defer Mutex.unlock(files);
        match files.value |> OrdMap.get_mut(&uri_str) with (
            | :Some file => (
                let latest_version = Mutex.lock(&file^^.latest_version);
                @defer Mutex.unlock(latest_version);
                latest_version.value^ += 1;
                { file^, latest_version.value^ }
            )
            | :None => (
                let version = 0;
                let file = Rc.new({
                    .latest_version = Mutex.new(version),
                    .processed = Mutex.new(:None),
                    .path = :Uri uri,
                });
                files.value |> OrdMap.add(uri_str, file);
                { file, version }
            )
        )
    );
    let processed = Mutex.lock(&file^.processed);
    @defer Mutex.unlock(processed);
    let is_latest_version = (
        let latest_version = Mutex.lock(&file^.latest_version);
        @defer Mutex.unlock(latest_version);
        latest_version.value^ == my_version
    );
    if is_latest_version then (
        processed.value^ = :Some Rc.new(do_process(state, uri, contents, my_version));
    );
);

const do_process = (
    state :: &State,
    uri :: Rc.t[Uri],
    contents :: &RcStr,
    version :: FileVersion,
) -> ProcessedFile => (
    let contents = contents^;
    let source :: Source = { .path = :Uri uri, .contents };
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
        let is_mks = match get_ext(&uri^.path |> as_str) with (
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
    {
        .source,
        .lines = (
            let mut lines = ArrayList.new();
            for line in RcStr.lines(&contents) do (
                &mut lines |> ArrayList.push_back(line);
            );
            lines
        ),
        .parsed,
        .diagnostics,
        .version,
    }
);

const did_open = (state :: &State, notifitication :: Json.t) -> () => (
    let :Object ref fields = notifitication;
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

const did_change = (state :: &State, notifitication :: Json.t) -> () => (
    let :Object ref fields = notifitication;
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
