use (import "./output.ks").*;
use (import "./span.ks").*;
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
                output.write(" at ");
                &self^.span |> Span.print;
            )
        );
    );

    const print = token => Token.print_impl(token, .verbose = false);

    const RawStringPart = newtype (
        | :Escape {
            .raw :: String,
            .span :: Span,
        }
        | :Content {
            .raw :: String,
            .span :: Span,
        }
    );

    const InterpolatedStringPart = newtype (
        | :Content {
            .raw :: String,
            .raw_parts :: ArrayList.t[RawStringPart],
            .contents :: String,
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
        .delimiter :: String,
        .raw :: String,
        .open :: Token.t,
        .parts :: ArrayList.t[InterpolatedStringPart],
        .close :: Token.t,
        .stripped_indentation :: String,
    };

    const NumberToken = newtype {
        .raw :: String,
    };

    const StringToken = newtype {
        .raw :: String,
        .open :: Token.t,
        .raw_parts :: ArrayList.t[RawStringPart],
        .close :: Token.t,
        .contents :: String,
        .stripped_indentation :: String,
    };

    const raw = (token :: &Token.t) -> &str => (
        Token.Shape.raw(&token^.shape^)
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
                .string :: Option.t[type { StringToken, .at_token :: Token.t }],
                .name :: String,
            }
            | :String StringToken
            | :InterpolatedString InterpolatedStringShape
            | :Number NumberToken
            | :Eof
            | :Error {
                .raw :: String,
            }
        );

        const clone = (self :: &Token.Shape.t) -> Token.Shape.t => (
            self^
        );

        const raw = (self :: &Token.Shape.t) -> &str => match self^ with (
            | :Punct { .raw = ref raw, ... } => raw |> as_str
            | :Comment { .raw = ref raw, ... } => raw |> as_str
            | :Ident { .raw = ref raw, ... } => raw |> as_str
            | :String { .raw = ref raw, ... } => raw |> as_str
            | :InterpolatedString { .raw = ref raw, ... } => raw |> as_str
            | :Number { .raw = ref raw, ... } => raw |> as_str
            | :Error { .raw = ref raw, ... } => raw |> as_str
            | :Eof => ""
        );

        const print_impl = (
            self :: &Token.Shape.t,
            .verbose :: Bool,
        ) => (
            let output = @current Output;
            match self^ with (
                | :Comment { .raw = ref raw, ... } => (
                    let raw = raw |> as_str;
                    for c in String.iter(raw) do (
                        if c == '\n' then (
                            ansi.with_mode(
                                :Cyan,
                                () => output.write("\\n"),
                            );
                        ) else (
                            ansi.with_mode(
                                :Gray,
                                () => output.write(&to_string(c) |> as_str),
                            );
                        );
                    );
                )
                | :Punct { .raw = ref raw, ... } => (
                    let raw = raw |> as_str;
                    ansi.with_mode(
                        :Magenta,
                        () => output.write(raw),
                    );
                )
                | :Ident {
                    .raw = ref raw,
                    .name = ref name,
                    ...
                } => (
                    let raw = raw |> as_str;
                    let name = name |> as_str;
                    if not verbose or raw == name then (
                        ansi.with_mode(
                            :Under,
                            () => output.write(raw),
                        );
                    ) else (
                        ansi.with_mode(
                            :Under,
                            () => output.write(raw),
                        );
                        output.write(" = ");
                        ansi.with_mode(
                            :Green,
                            () => output.write(&String.escape(name) |> as_str),
                        );
                    );
                )
                | :Number { .raw = ref raw, ... } => (
                    let raw = raw |> as_str;
                    ansi.with_mode(
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :String {
                    .raw = ref raw,
                    .contents = ref contents,
                    ...
                } => (
                    let raw = raw |> as_str;
                    let contents = contents |> as_str;
                    ansi.with_mode(
                        :Green,
                        () => output.write(raw),
                    );
                    if verbose then (
                        output.write(" {\n");
                        output.inc_indentation();
                        output.write(".contents = ");
                        ansi.with_mode(
                            :Green,
                            () => output.write(&String.escape(contents) |> as_str),
                        );
                        output.write("\n");
                        output.dec_indentation();
                        output.write("}");
                    );
                )
                | :InterpolatedString {
                    .delimiter = ref delimiter,
                    .parts = ref parts,
                    ...
                } => (
                    let delimiter = delimiter |> as_str;
                    if verbose then (
                        ansi.with_mode(
                            :Green,
                            () => (
                                output.write(delimiter);
                                output.write("interpolated");
                                output.write(delimiter);
                            ),
                        );
                        output.write(" {\n");
                        output.inc_indentation();
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => output.write(delimiter),
                        );
                    );
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content {
                                .raw = ref raw,
                                .contents = ref contents,
                                ...
                            } => (
                                let raw = raw |> as_str;
                                let contents = contents |> as_str;
                                if verbose then (
                                    ansi.with_mode(
                                        :Green,
                                        () => (
                                            output.write(delimiter);
                                            output.write(raw);
                                            output.write(delimiter);
                                        ),
                                    );
                                    output.write(" {\n");
                                    output.inc_indentation();
                                    output.write(".contents = ");
                                    ansi.with_mode(
                                        :Green,
                                        () => output.write(&String.escape(contents) |> as_str),
                                    );
                                    output.write("\n");
                                    output.dec_indentation();
                                    output.write("}");
                                ) else (
                                    ansi.with_mode(
                                        :Green,
                                        () => output.write(raw),
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
                                        () => output.write("\\"),
                                    );
                                    output.write(" {\n");
                                    output.inc_indentation();
                                    for token in tokens |> ArrayList.iter do (
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                        output.write("\n");
                                    );
                                    output.dec_indentation();
                                    output.write("}");
                                ) else (
                                    ansi.with_mode(
                                        :Cyan,
                                        () => output.write("\\("),
                                    );
                                    let mut first = true;
                                    for token in tokens |> ArrayList.iter do (
                                        if first then (
                                            first = false;
                                        ) else (
                                            output.write(" ");
                                        );
                                        Token.Shape.print_impl(&token^.shape^, .verbose);
                                    );
                                    ansi.with_mode(
                                        :Cyan,
                                        () => output.write(")"),
                                    );
                                );
                            )
                        );
                        if verbose then (
                            output.write("\n");
                        );
                    );
                    if verbose then (
                        output.dec_indentation();
                        output.write("}");
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => output.write(delimiter),
                        );
                    );
                )
                | :Error { .raw = ref raw, ... } => (
                    let raw = raw |> as_str;
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

        const print = shape => print_impl(shape, .verbose = false);
    );
);
