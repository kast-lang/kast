use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../log.ks").*;
use (import "../serialize.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token.ks").*;
use (import "../token_stream.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../json/_lib.ks").*;

module:

const Tokenize = (
    module:

    const Args = (
        module:

        const t = newtype {
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            for i in start_index..std.sys.argc() do (
                &mut paths |> ArrayList.push_back(std.sys.argv_at(i));
            );
            {
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let process = (path :: SourcePath) => (
            Log.info(
                () => ansi.with_mode(
                    :Bold,
                    () => (@current Output).write("Lexing " + to_string(path) + "\n\n"),
                ),
            );
            match common_args.output_mode with (
                | :Human => (
                    let mut lexer = Lexer.new(Source.read(path));
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        token |> Token.print_impl(.verbose = true);
                        (@current Output).write("\n");
                        if token.shape is :Eof then break;
                    );
                )
                | :Json => (
                    let mut lexer = Lexer.new(Source.read(path));
                    let mut json_tokens = ArrayList.new();
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        &mut json_tokens |> ArrayList.push_back(Serialize.as_json(token));
                        if token.shape is :Eof then break;
                    );
                    let json = :Array json_tokens;
                    Json.print(&json);
                    (@current Output).write("\n");
                )
            );
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.parse(path));
        );
    );
);
