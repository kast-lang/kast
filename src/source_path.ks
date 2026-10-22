use (import "../deps/uri/src/lib.ks").*;
use (import "./output.ks").*;
use std.rc.Rc;

module:

const SourcePath = newtype (
    | :Stdin
    | :Uri Rc.t[Uri]
    | :Special Rc.t[String]
);

impl SourcePath as module = (
    module:

    const parse = (s :: &str) -> SourcePath => (
        let mut uri :: Uri = String.parse(s);
        if &uri.scheme |> as_str == "kast" then (
            match std.sys.get_env("KAST_PATH") with (
                | :Some kast_path => (
                    # TODO resolve to absolute
                    let mut new_uri = Uri.spec_path("file", &kast_path |> as_str);
                    new_uri.path = String.concat_owned(new_uri.path, uri.path);
                    uri = new_uri;
                )
                | :None => panic("KAST_PATH not set")
            )
        ) else if &uri.scheme |> as_str == "" then (
            uri.scheme = String.from_str("file");
        );
        :Uri Rc.new(uri)
    );

    const file = (path :: &str) -> SourcePath => (
        :Uri Rc.new(
            # TODO resolve to absolute
            Uri.spec_path("file", path)
        )
    );

    const file_path = (path :: &SourcePath) -> Option.t[&str] => (
        match path^ with (
            | :Uri uri => (
                if &uri^.scheme |> as_str == "file" then (
                    :Some (&uri^.path |> as_str)
                ) else (
                    :None
                )
            )
            | _ => :None
        )
    );

    const print = (self :: &SourcePath) => match self^ with (
        | :Stdin => (
            write_str("<stdin>");
        )
        | :Uri uri => if &uri^.scheme |> as_str == "file" then (
            # TODO relative to cwd?
            write_str(&uri^.path |> as_str);
        ) else (
            write_str(&Uri.to_string(&uri^) |> as_str);
        )
        | :Special name => (
            write_str(&name^ |> as_str);
        )
    );
);

