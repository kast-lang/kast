use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../json/_lib.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
const dep_json = import "../../deps/json/lib.ks";

module:

const ParseJson = (
    module:

    const Args = (
        module:

        const t = newtype {
            .use_kast_parser :: Bool,
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut use_kast_parser = false;
            let mut paths = ArrayList.new();
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--use-kast-parser" then (
                    use_kast_parser = true;
                    i += 1;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.path_arg(arg, .ext = :Some "json"));
                i += 1;
            );
            {
                .use_kast_parser,
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let process = (path :: SourcePath) => (
            let source = Source.read(path);
            let json = if args.use_kast_parser then (
                let json = Json.parse(source.contents)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                json
            ) else (
                let mut reader = dep_json.Reader.create(&source.contents);
                let json = dep_json.parse(&mut reader)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                Json.from_dep(json)
            );
            Json.print(&json);
            (@current Output).write("\n");
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
    );
);
