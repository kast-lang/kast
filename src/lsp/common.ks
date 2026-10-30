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
