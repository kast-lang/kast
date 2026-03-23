use (import "./error.ks").*;
use (import "./output.ks").*;
use (import "./position.ks").*;
use (import "./source.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./parser.ks").*;
use (import "./json.ks").*;

with Output = stdout();

const Args = (
    module:
    
    const LexerArgs = (
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
    
    const ParseSyntaxRulesArgs = (
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
    
    const ParseArgs = (
        module:
        
        const t = newtype {
            .ruleset_path :: Option.t[String],
            .paths :: ArrayList.t[String],
        };
        
        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            let mut i = start_index;
            let mut ruleset_path = :None;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--ruleset" then (
                    if i + 1 >= std.sys.argc() then (
                        panic("Expected ruleset path");
                    );
                    ruleset_path = :Some std.sys.argv_at(i + 1);
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(arg);
                i += 1;
            );
            {
                .ruleset_path,
                .paths,
            }
        );
    );
    
    const ParseJsonArgs = (
        module:
        
        const t = newtype {
            .path :: Option.t[String],
        };
        
        const parse = start_index -> t => (
            let mut path = :None;
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if path is :Some _ then (
                    panic("Only 1 arg is expected to parse-json");
                );
                path = :Some arg;
                i += 1;
            );
            {
                .path,
            }
        );
    );
    
    const Subcommand = newtype (
        | :Tokenize LexerArgs.t
        | :ParseSyntaxRules ParseSyntaxRulesArgs.t
        | :ParseSyntaxRuleset ParseSyntaxRulesArgs.t
        | :Parse ParseArgs.t
        | :ParseJson ParseJsonArgs.t
    );
    
    const t = newtype {
        .subcommand :: Subcommand,
        .stop_on_error :: Bool,
    };
    
    const parse = () -> t => (
        let mut stop_on_error = true;
        let subcommand = unwindable subcommand (
            for i in 1..std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "lex" or arg == "tokenize" then (
                    unwind subcommand (:Tokenize LexerArgs.parse(i + 1));
                );
                if arg == "parse_syntax_rules" then (
                    unwind subcommand (:ParseSyntaxRules ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse_syntax_ruleset" then (
                    unwind subcommand (:ParseSyntaxRuleset ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse" then (
                    unwind subcommand (:Parse ParseArgs.parse(i + 1));
                );
                if arg == "parse-json" then (
                    unwind subcommand (:ParseJson ParseJsonArgs.parse(i + 1));
                );
                if arg == "--continue-on-error" then (
                    stop_on_error = false;
                    continue;
                );
                panic("Unexpected arg " + arg);
            );
            panic("No default subcommand")
        );
        {
            .stop_on_error,
            .subcommand,
        }
    );
);

let args = Args.parse();
with Error.HandlerContext = Error.init_handler(.stop_on_error = args.stop_on_error);
let output = @current Output;
match args.subcommand with (
    | :Tokenize { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Lexing " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            loop (
                let token = &mut lexer |> Lexer.next;
                token |> Token.print;
                output.write("\n");
                if token.shape is :Eof then break;
            );
        );
    )
    | :ParseSyntaxRules { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            dbg.print(rules);
        );
    )
    | :ParseSyntaxRuleset { .paths } => (
        let mut ruleset = SyntaxRuleset.new();
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            for &rule in ArrayList.iter(&rules) do (
                &mut ruleset |> SyntaxRuleset.add(rule);
            );
        );
        SyntaxRuleset.print(&ruleset);
    )
    | :Parse { .ruleset_path, .paths } => (
        let ruleset_path = ruleset_path |> Option.unwrap_or("tests/syntax/kast.ks");
        ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + ruleset_path + "\n\n"));
        let mut lexer = Lexer.new(Source.read_file(ruleset_path));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing " + path + "\n\n"));
            let source = Source.read_file(path);
            let entire_source_span = (
                let start = Position.beginning();
                let mut end = start;
                for c in source.contents |> String.iter do (
                    &mut end |> Position.advance(c);
                );
                {
                    .start,
                    .end,
                    .uri = source.uri,
                }
            );
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span,
                .uri = source.uri,
                .token_stream = &mut token_stream,
            );

            Ast.print(&parsed.ast);
        );
    )
    | :ParseJson { .path } => (
        let read_stdin = () => panic("TODO read stdin");
        let json = match path with (
            | :Some path => std.fs.read_file(path)
            | :None => read_stdin()
        );
        match Json.parse(json) with (
            | :Ok json => (
                Json.print(&json);
                (@current Output).write("\n");
            )
            | :Error { .position, .message } => (
                panic("TODO");
            )
        );
    )
);
