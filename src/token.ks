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
        let output = @current Output;
        &self^.shape^ |> Token.Shape.print_impl(.verbose);
        ansi.with_mode(
            :Dim,
            () => (
                write_str(" at ");
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
            let output = @current Output;
            match self^ with (
                | :Comment { .raw = ref raw, ... } => (
                    for c in RcStr.iter(raw) do (
                        if c == '\n' then (
                            ansi.with_mode(
                                :Cyan,
                                () => write_str("\\n"),
                            );
                        ) else (
                            ansi.with_mode(
                                :Gray,
                                () => write_str(&to_string(c) |> as_str),
                            );
                        );
                    );
                )
                | :Punct { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Magenta,
                        () => write(raw),
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
                            () => write(raw),
                        );
                    ) else (
                        ansi.with_mode(
                            :Under,
                            () => write(raw),
                        );
                        write_str(" = ");
                        ansi.with_mode(
                            :Green,
                            () => write_escaped(name),
                        );
                    );
                )
                | :Number { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => write(raw),
                    );
                )
                | :String {
                    .raw = ref raw,
                    .contents = ref contents,
                    ...
                } => (
                    ansi.with_mode(
                        :Green,
                        () => write(raw),
                    );
                    if verbose then (
                        write_str(" {\n");
                        output.inc_indentation();
                        write_str(".contents = ");
                        ansi.with_mode(
                            :Green,
                            () => write_escaped(contents),
                        );
                        write_str("\n");
                        output.dec_indentation();
                        write_str("}");
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
                                write(delimiter);
                                write_str("interpolated");
                                write(delimiter);
                            ),
                        );
                        write_str(" {\n");
                        output.inc_indentation();
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => write(delimiter),
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
                                            write(delimiter);
                                            write(raw);
                                            write(delimiter);
                                        ),
                                    );
                                    write_str(" {\n");
                                    output.inc_indentation();
                                    write_str(".contents = ");
                                    ansi.with_mode(
                                        :Green,
                                        () => write_escaped(contents),
                                    );
                                    write_str("\n");
                                    output.dec_indentation();
                                    write_str("}");
                                ) else (
                                    ansi.with_mode(
                                        :Green,
                                        () => write(raw),
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
                                        () => write_str("\\"),
                                    );
                                    write_str(" {\n");
                                    output.inc_indentation();
                                    for token in tokens |> ArrayList.iter do (
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                        write_str("\n");
                                    );
                                    output.dec_indentation();
                                    write_str("}");
                                ) else (
                                    ansi.with_mode(
                                        :Cyan,
                                        () => write_str("\\("),
                                    );
                                    let mut first = true;
                                    for token in tokens |> ArrayList.iter do (
                                        if first then (
                                            first = false;
                                        ) else (
                                            write_str(" ");
                                        );
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                    );
                                    ansi.with_mode(
                                        :Cyan,
                                        () => write_str(")"),
                                    );
                                );
                            )
                        );
                        if verbose then (
                            write_str("\n");
                        );
                    );
                    if verbose then (
                        output.dec_indentation();
                        write_str("}");
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => write(delimiter),
                        );
                    );
                )
                | :Error { .raw = ref raw, ... } => (
                    ansi.with_mode(
                        :Red,
                        () => write(raw),
                    );
                )
                | :Eof => (
                    ansi.with_mode(
                        :Italic,
                        () => write_str("<eof>"),
                    );
                )
            );
        );

        const print = shape => print_impl(shape, .verbose = false);
    );
);
