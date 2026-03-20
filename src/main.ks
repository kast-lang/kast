use (import "./error.ks").*;
use (import "./ansi.ks").*;
use (import "./output.ks").*;
use (import "./source.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;

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
    
    const Subcommand = newtype (
        | :Tokenize LexerArgs.t
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
                let token = &lexer |> Lexer.peek;
                # THIS IS HACK BECAUSE OF BUG IN KAST
                let f :: Token -> () = Token.print;
                token |> f;
                (@current Output).write("\n");
                if token.shape is :Eof then break;
                Lexer.advance(&mut lexer);
            );
        );
    )
);
