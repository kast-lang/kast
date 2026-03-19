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
        output.write(" at ");
        self.span |> Span.print;
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
    }
    | :Eof
);

impl TokenShape as module = (
    module:
    
    const raw = (self :: TokenShape) -> String => match self with (
        | :Punct { .raw, ... } => (
            raw
        )
        | :Ident { .raw, ... } => (
            raw
        )
        | :String { .raw, ... } => (
            raw
        )
        | :Eof => (
            ""
        
        )
    );
    
    const print = (self :: TokenShape) => (
        let output = @current Output;
        match self with (
            | :Punct { .raw, ... } => (
                output.write(raw);
            )
            | :Ident { .raw, ... } => (
                output.write(raw);
            )
            | :String { .raw, ... } => (
                output.write(raw);
            )
            | :Eof => (
                output.write(":Eof");
            )
        );
    );
);
