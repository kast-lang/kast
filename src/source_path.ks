use (import "../deps/uri/src/lib.ks").*;

module:

const SourcePath = newtype (
    | :Stdin
    | :Uri Uri
    | :Special String
);

impl SourcePath as module = (
    module:

    const parse = (s :: String) -> SourcePath => (
        let mut uri :: Uri = String.parse(s);
        if uri.scheme == "kast" then (
            match std.sys.get_env("KAST_PATH") with (
                | :Some kast_path => (
                    # TODO resolve to absolute
                    let mut new_uri = Uri.spec_path("file", kast_path);
                    new_uri.path += uri.path;
                    uri = new_uri;
                )
                | :None => panic("KAST_PATH not set")
            )
        ) else if uri.scheme == "" then (
            uri.scheme = "file";
        );
        :Uri uri
    );

    const file = (path :: String) -> SourcePath => (
        :Uri (
            # TODO resolve to absolute
            Uri.spec_path("file", path)
        )
    );

    const file_path = (path :: SourcePath) -> Option.t[String] => (
        match path with (
            | :Uri uri => (
                if uri.scheme == "file" then (
                    :Some uri.path
                ) else (
                    :None
                )
            )
            | _ => :None
        )
    );
);

impl SourcePath as ToString = {
    .to_string = (self :: SourcePath) => match self with (
        | :Stdin => "<stdin>"
        | :Uri uri => if uri.scheme == "file" then (
            # TODO relative to cwd?
            uri.path
        ) else (
            Uri.to_string(uri)
        )
        | :Special name => name
    ),
};
