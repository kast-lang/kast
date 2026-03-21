use (import "./common.ks").*;
use (import "./ansi.ks").*;
use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Token = (
    module:
    
    const t = newtype {
        .shape :: Token.Shape.t,
        .span :: Span,
    };
    
    const print = (self :: Token.t) => (
        let output = @current Output;
        self.shape |> Token.Shape.print;
        ansi.with_mode(
            :Dim,
            () => (
                output.write(" at ");
                self.span |> Span.print;
            )
        );
    );
    
    const Shape = (
        module:
        
        const t = newtype (
            | :Comment {
                .raw :: String,
                .ty :: (:Line | :Block),
            }
            | :Punct {
                .raw :: String,
            }
            | :Ident {
                .raw :: String,
                .name :: String,
            }
            | :String {
                .delimeter :: String,
                .raw :: String,
                .contents :: String,
            }
            | :Number {
                .raw :: String,
            }
            | :Eof
            | :Error {
                .raw :: String,
            }
        );
        
        const raw = (self :: Token.Shape.t) -> String => match self with (
            | :Punct { .raw, ... } => raw
            | :Comment { .raw, ... } => raw
            | :Ident { .raw, ... } => raw
            | :String { .raw, ... } => raw
            | :Number { .raw, ... } => raw
            | :Error { .raw, ... } => raw
            | :Eof => ""
        );
        
        const print = (self :: Token.Shape.t) => (
            let output = @current Output;
            match self with (
                | :Comment { .raw, ... } => (
                    for c in String.iter(raw) do (
                        if c == '\n' then (
                            ansi.with_mode(
                                :Cyan,
                                () => output.write("\\n"),
                            );
                        ) else (
                            ansi.with_mode(
                                :Gray,
                                () => output.write(to_string(c)),
                            );
                        );
                    );
                )
                | :Punct { .raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :Ident { .raw, ... } => (
                    ansi.with_mode(
                        :Under,
                        () => output.write(raw),
                    );
                )
                | :Number { .raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :String { .raw, .delimeter, .contents, ... } => (
                    ansi.with_mode(
                        :Green,
                        () => output.write(raw),
                    );
                    ansi.with_mode(
                        :Yellow,
                        () => output.write(" contents="),
                    );
                    ansi.with_mode(
                        :Green,
                        () => output.write(escape_string_with(contents, .delimeter = "\"")),
                    );
                )
                | :Error { .raw, ... } => (
                    ansi.with_mode(
                        :Red,
                        () => output.write(raw),
                    );
                )
                | :Eof => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write("<eof>"),
                    );
                )
            );
        );
    );
);
