use (import "./error.ks").*;
use (import "./ansi.ks").*;
use (import "./output.ks").*;
use (import "./source.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./syntax_ruleset.ks").*;

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
    
    const Subcommand = newtype (
        | :Tokenize LexerArgs.t
        | :ParseSyntaxRules ParseSyntaxRulesArgs.t
        | :ParseSyntaxRuleset ParseSyntaxRulesArgs.t
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
match args.subcommand with (
    | :Tokenize { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => write("Lexing " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            loop (
                let token = &mut lexer |> Lexer.next;
                token |> Token.print;
                (@current Output).write("\n");
                if token.shape is :Eof then break;
            );
        );
    )
    | :ParseSyntaxRules { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            dbg.print(rules);
        );
    )
    | :ParseSyntaxRuleset { .paths } => (
        let mut ruleset = SyntaxRuleset.new();
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            for &rule in ArrayList.iter(&rules) do (
                &mut ruleset |> SyntaxRuleset.add(rule);
            );
        );
        SyntaxRuleset.print(&ruleset);
    )
);
