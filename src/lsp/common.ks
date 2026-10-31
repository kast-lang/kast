module:

use (import "../diagnostic.ks").*;
use (import "../log.ks").*;
use (import "../ast.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../position.ks").*;
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../json/_lib.ks").*;
use (import "../highlight.ks").*;
use (import "../format.ks").*;
use (import "../json_rpc.ks").*;
use (import "../rc_str.ks").*;
use (import "../../deps/uri/src/lib.ks").*;
use std.collections.OrdMap;
use std.rc.Rc;
use std.sync.Mutex;

const path_to_uri_lsp = (path :: SourcePath) -> Json.t => (
    let uri_str = match path with (
        | :Stdin => format!("stdin://")
        | :Uri ref uri => Uri.to_string(&uri^^)
        | :Special ref s => format!("special://\(s^)")
    );
    :String RcStr.from_String(uri_str)
);

const span_to_lsp_range = (span :: Span) -> Json.t => (
    let mut fields = OrdMap.new();
    &mut fields |> OrdMap.add(RcStr.allocate("start"), position_to_lsp(span.start));
    &mut fields |> OrdMap.add(RcStr.allocate("end"), position_to_lsp(span.end));
    :Object fields
);

const span_to_lsp_location = (span :: Span) -> Json.t => (
    let mut fields = OrdMap.new();
    &mut fields |> OrdMap.add(RcStr.allocate("range"), span_to_lsp_range(span));
    let uri = match span.path with (
        | :Uri ref uri => :String RcStr.from_String(Uri.to_string(&uri^^))
        | _ => panic_fmt!("Was trying to report to lsp this path which is not uri: \(span.path)")
    );
    &mut fields |> OrdMap.add(RcStr.allocate("uri"), uri);
    :Object fields
);

const position_from_lsp = (position :: &Json.t) -> Position => (
    let :Object ref fields = position^;
    let (&(:Number line)) = fields
        |> OrdMap.get(&RcStr.allocate("line"))
        |> Option.unwrap;
    let (&(:Number column)) = fields
        |> OrdMap.get(&RcStr.allocate("character"))
        |> Option.unwrap;
    let line = std.convert.float64_to_int32(line);
    let column = std.convert.float64_to_int32(column);
    # TODO zeros are because we dont use them, but maybe should initialize them still?
    {
        .string_encoding_index = 0,
        .line,
        .column = {
            .string_encoding = column,
            .display_width = 0,
        },
    }
);

const position_to_lsp = (position :: Position) -> Json.t => (
    let mut fields = OrdMap.new();
    let line :: Json.t = :Number (
        std.convert.int32_to_float64(position.line)
    );
    &mut fields |> OrdMap.add(RcStr.allocate("line"), line);
    let character :: Json.t = :Number (
        std.convert.int32_to_float64(position.column.string_encoding)
    );
    &mut fields |> OrdMap.add(RcStr.allocate("character"), character);
    :Object fields
);

const iter_ast_from_span_outwards = (
    ast :: &Ast.t,
    span :: Span,
) -> std.iter.Iterable[type (&Ast.t)] => (
    const @"impl" = (
        module:

        const Context = @context newtype {
            .span :: Span,
            .consumer :: &Ast.t -> (),
        };

        const walk_group = (group :: &Ast.Group) => (
            walk_parts(&group^.parts);
        );

        const walk_parts = (parts :: &ArrayList.t[Ast.Part]) => (
            for part in ArrayList.iter(parts) do (
                match part^ with (
                    | :Ignored _ => ()
                    | :Keyword _ => ()
                    | :Value ref ast => walk_ast(&ast^^)
                    | :Group ref group => walk_group(&group^^)
                );
            );
        );

        const walk_ast = (ast :: &Ast.t) => with_return (
            let ref ctx = @current Context;
            if not (ast^.span |> Span.contains(ctx^.span)) then (
                return;
            );
            match ast^.shape^ with (
                | :Empty => ()
                | :Token _ => ()
                | :InterpolatedString {
                    .parts = ref parts,
                    ...
                } => (
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content _ => ()
                            | :Interpolated { .ast = ref ast, ... } => (
                                walk_ast(ast);
                            )
                        );
                    );
                )
                | :Rule {
                    .rule = _,
                    .root = ref root,
                } => walk_group(root)
                | :Syntax {
                    .command = _,
                    .value_after = ref value_after,
                } => (
                    if value_after^ is :Some ref value_after then (
                        walk_ast(value_after);
                    );
                )
                | :Error { .parts = ref parts } => (
                    walk_parts(parts);
                )
            );
            ctx^.consumer(ast);
        );
    );
    {
        .iter = @move consumer => (
            with @"impl".Context = {
                .span,
                .consumer,
            };
            @"impl".walk_ast(ast);
        ),
    }
);
