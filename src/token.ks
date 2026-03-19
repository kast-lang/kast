use (import "./span.ks").*;

module:

const Token = newtype {
    .shape :: TokenShape,
    .span :: Span,
};

const TokenShape = newtype (
    | :Raw String
    | :Eof
);
