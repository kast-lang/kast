use (import "./common.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../log.ks").*;
# use (import "../serialize.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token.ks").*;
use (import "../token_stream.ks").*;
use (import "../lexer/_lib.ks").*;
# use (import "../json/_lib.ks").*;

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
                &mut paths |> ArrayList.push_back(String.from_str(std.sys.argv_at(i)));
            );
            {
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let process = (path :: &SourcePath) => (
            Log.info_fn(() => (
                locked_output.write_str("Lexing ");
                SourcePath.print(path);
                locked_output.write_str("\n\n");
            ));
            match common_args.output_mode with (
                | :None => (
                    let source = Source.read(path);
                    let mut lexer = Lexer.new(source);
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        if token.shape^ is :Eof then break;
                    );
                )
                | :Human => (
                    let source = Source.read(path);
                    let mut lexer = Lexer.new(source);
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        lock_output(() => (
                            &token |> Token.print_impl(.verbose = true);
                            locked_output.write_str("\n");
                        ));
                        if token.shape^ is :Eof then break;
                    );
                )
                | :Json => (
                    Diagnostic.abort("json is broken :)");
                    @comment_out (
                        let mut lexer = Lexer.new(Source.read(path));
                        let mut json_tokens = ArrayList.new();
                        loop (
                            let token = &mut lexer |> Lexer.next;
                            &mut json_tokens |> ArrayList.push_back(Serialize.as_json(token));
                            if token.shape^ is :Eof then break;
                        );
                        let json = :Array json_tokens;
                        lock_output(() => (
                            Json.print(&json);
                            locked_output.write("\n");
                        ));
                    );
                )
            );
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(&(:Stdin));
        );
        for path in args.paths |> ArrayList.into_iter do (
            let path = &path |> as_str;
            process(&SourcePath.parse(path));
        );
    );
);
