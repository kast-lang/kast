use (import "./output.ks").*;
use (import "./span.ks").*;
use (import "./rc_str.ks").*;
use std.rc.Rc;

module:

const Token = (
    module:

    const t = newtype {
        .shape :: Rc.t[Token.Shape.t],
        .span :: Span,
    };

    const print_impl = (
        self :: &Token.t,
        .verbose :: Bool,
    ) => (
        &self^.shape^ |> Token.Shape.print_impl(.verbose);
        ansi.with_mode(
            :Dim,
            () => (
                locked_output.write_str(" at ");
                &self^.span |> Span.print;
            )
        );
    );

    const print = token => Token.print_impl(token, .verbose = false);

    const RawStringPart = newtype (
        | :Escape {
            .raw :: RcStr,
            .span :: Span,
        }
        | :Content {
            .raw :: RcStr,
            .span :: Span,
        }
    );

    const InterpolatedStringPart = newtype (
        | :Content {
            .raw :: RcStr,
            .raw_parts :: ArrayList.t[RawStringPart],
            .contents :: RcStr,
            .span :: Span,
        }
        | :Interpolated {
            .open :: Token.t,
            .close :: Token.t,
            .tokens :: ArrayList.t[Token.t],
            .span :: Span,
        }
    );

    const InterpolatedStringShape = newtype {
        .delimiter :: RcStr,
        .raw :: RcStr,
        .open :: Token.t,
        .parts :: ArrayList.t[InterpolatedStringPart],
        .close :: Token.t,
        .stripped_indentation :: RcStr,
    };

    const NumberToken = newtype {
        .raw :: RcStr,
    };

    const StringToken = newtype {
        .raw :: RcStr,
        .open :: Token.t,
        .raw_parts :: ArrayList.t[RawStringPart],
        .close :: Token.t,
        .contents :: RcStr,
        .stripped_indentation :: RcStr,
    };

    const raw = (token :: &Token.t) -> &str => (
        Token.Shape.raw(&token^.shape^)
    );

    const Shape = (
        module:

        const t = newtype (
            | :Comment {
                .raw :: RcStr,
                .ty :: (:Line | :Block),
            }
            | :Punct {
                .raw :: RcStr,
            }
            | :Ident {
                .raw :: RcStr,
                .string :: Option.t[type { StringToken, .at_token :: Token.t }],
                .name :: RcStr,
            }
            | :String StringToken
            | :InterpolatedString InterpolatedStringShape
            | :Number NumberToken
            | :Eof
            | :Error {
                .raw :: RcStr,
            }
        );

        const clone = (self :: &Token.Shape.t) -> Token.Shape.t => (
            self^
        );

        const raw = (self :: &Token.Shape.t) -> &str => match self^ with (
            | :Punct { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :Comment { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :Ident { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :String { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :InterpolatedString { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :Number { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :Error { .raw = ref raw, ... } => raw |> RcStr.as_str
            | :Eof => ""
        );

        const print_impl = (
            self :: &Token.Shape.t,
            .verbose :: Bool,
        ) => (
            match self^ with (
                | :Comment { .raw = ref raw, ... } => (
                    for c in RcStr.iter(raw) do (
                        if c == '\n' then (
                            ansi.with_mode(
                                :Cyan,
                                () => locked_output.write_str("\\n"),
                            );
                        ) else (
                            ansi.with_mode(
                                :Gray,
                                () => locked_output.write_str(&to_string(c) |> as_str),
                            );
                        );
                    );
                )
                | :Punct { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Magenta,
                        () => locked_output.write(raw),
                    );
                )
                | :Ident {
                    .raw = ref raw,
                    .name = ref name,
                    ...
                } => (
                    if not verbose or raw == name then (
                        ansi.with_mode(
                            :Under,
                            () => locked_output.write(raw),
                        );
                    ) else (
                        ansi.with_mode(
                            :Under,
                            () => locked_output.write(raw),
                        );
                        locked_output.write_str(" = ");
                        ansi.with_mode(
                            :Green,
                            () => locked_output.write_escaped(name),
                        );
                    );
                )
                | :Number { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => locked_output.write(raw),
                    );
                )
                | :String {
                    .raw = ref raw,
                    .contents = ref contents,
                    ...
                } => (
                    ansi.with_mode(
                        :Green,
                        () => locked_output.write(raw),
                    );
                    if verbose then (
                        locked_output.write_str(" {\n");
                        locked_output.inc_indentation();
                        locked_output.write_str(".contents = ");
                        ansi.with_mode(
                            :Green,
                            () => locked_output.write_escaped(contents),
                        );
                        locked_output.write_str("\n");
                        locked_output.dec_indentation();
                        locked_output.write_str("}");
                    );
                )
                | :InterpolatedString {
                    .delimiter = ref delimiter,
                    .parts = ref parts,
                    ...
                } => (
                    if verbose then (
                        ansi.with_mode(
                            :Green,
                            () => (
                                locked_output.write(delimiter);
                                locked_output.write_str("interpolated");
                                locked_output.write(delimiter);
                            ),
                        );
                        locked_output.write_str(" {\n");
                        locked_output.inc_indentation();
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => locked_output.write(delimiter),
                        );
                    );
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content {
                                .raw = ref raw,
                                .contents = ref contents,
                                ...
                            } => (
                                if verbose then (
                                    ansi.with_mode(
                                        :Green,
                                        () => (
                                            locked_output.write(delimiter);
                                            locked_output.write_escaped_contents(raw, .delimiter);
                                            locked_output.write(delimiter);
                                        ),
                                    );
                                    locked_output.write_str(" {\n");
                                    locked_output.inc_indentation();
                                    locked_output.write_str(".contents = ");
                                    ansi.with_mode(
                                        :Green,
                                        () => locked_output.write_escaped(contents),
                                    );
                                    locked_output.write_str("\n");
                                    locked_output.dec_indentation();
                                    locked_output.write_str("}");
                                ) else (
                                    ansi.with_mode(
                                        :Green,
                                        () => locked_output.write(raw),
                                    );
                                );
                            )
                            | :Interpolated {
                                .tokens = ref tokens,
                                ...
                            } => (
                                if verbose then (
                                    ansi.with_mode(
                                        :Cyan,
                                        () => locked_output.write_str("\\"),
                                    );
                                    locked_output.write_str(" {\n");
                                    locked_output.inc_indentation();
                                    for token in tokens |> ArrayList.iter do (
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                        locked_output.write_str("\n");
                                    );
                                    locked_output.dec_indentation();
                                    locked_output.write_str("}");
                                ) else (
                                    ansi.with_mode(
                                        :Cyan,
                                        () => locked_output.write_str("\\("),
                                    );
                                    let mut first = true;
                                    for token in tokens |> ArrayList.iter do (
                                        if first then (
                                            first = false;
                                        ) else (
                                            locked_output.write_str(" ");
                                        );
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                    );
                                    ansi.with_mode(
                                        :Cyan,
                                        () => locked_output.write_str(")"),
                                    );
                                );
                            )
                        );
                        if verbose then (
                            locked_output.write_str("\n");
                        );
                    );
                    if verbose then (
                        locked_output.dec_indentation();
                        locked_output.write_str("}");
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => locked_output.write(delimiter),
                        );
                    );
                )
                | :Error { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Red,
                        () => locked_output.write(raw),
                    );
                )
                | :Eof => (
                    ansi.with_mode(
                        :Italic,
                        () => locked_output.write_str("<eof>"),
                    );
                )
            );
        );

        const print = shape => print_impl(shape, .verbose = false);
    );
);
