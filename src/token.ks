use (import "./ansi.ks").*;
use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Token = newtype {
    .shape :: TokenShape,
    .span :: Span,
};

impl Token as module = (
    module:
    
    const print = (self :: Token) => (
        let output = @current Output;
        self.shape |> TokenShape.print;
        ansi.with_mode(
            :Dim,
            () => (
                output.write(" at ");
                self.span |> Span.print;
            )
        );
    );
);

const TokenShape = newtype (
    | :Punct {
        .raw :: String,
    }
    | :Ident {
        .raw :: String,
    }
    | :String {
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

impl TokenShape as module = (
    module:
    
    const raw = (self :: TokenShape) -> String => match self with (
        | :Punct { .raw, ... } => raw
        | :Ident { .raw, ... } => raw
        | :String { .raw, ... } => raw
        | :Number { .raw, ... } => raw
        | :Error { .raw, ... } => raw
        | :Eof => ""
    );
    
    const print = (self :: TokenShape) => (
        let output = @current Output;
        match self with (
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
            | :String { .raw, .contents, ... } => (
                ansi.with_mode(
                    :Green,
                    () => output.write(raw),
                );
                output.write("\n  contents=");
                ansi.with_mode(
                    :Green,
                    () => output.write(contents),
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
