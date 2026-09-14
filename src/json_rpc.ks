use (import "./json/_lib.ks").*;
const dep_json = import "../deps/json/lib.ks";
use std.collections.OrdMap;

module:

const BufferedReader = newtype {
    .read_exactly :: Int32 -> String,
    .read_until :: Char -> String,
};

const JsonRpc = (
    module:

    const Message = Json.t;

    const Io = newtype {
        .input :: BufferedReader,
        .output :: {
            .write :: String -> (),
        },
    };

    const stdio = () -> Io => {
        .input = {
            .read_exactly = std.io.stdin.read_exactly,
            .read_until = std.io.stdin.read_until,
        },
        .output = {
            .write = std.io.stdout.write,
        },
    };

    const Handler = newtype {
        .on_notification :: Json.t -> (),
        .on_request :: Json.t -> Result.t[Json.t, String],
    };

    const Header = newtype {
        .content_length :: Int32,
    };

    const read_header = (io :: Io) -> Header => (
        let mut content_length = :None;
        loop (
            let header = (
                let s = io.input.read_until('\n');
                if s |> String.at(String.length(s) - 1) != '\r' then (
                    panic("\\r expected");
                );
                let s = s |> String.substring(0, String.length(s) - 1);
                if s |> String.length == 0 then (
                    break;
                );
                let { name, value } = s |> String.split_once(':');
                if value |> String.at(0) != ' ' then (
                    panic("Expected space after :");
                );
                let value = value |> String.substring(1, String.length(value) - 1);
                { .name, .value }
            );
            if header.name == "Content-Length" then (
                if content_length is :Some _ then (
                    panic("Content-Length present multiple times???");
                );
                content_length = :Some parse(header.value);
            );
            if header.name == "Content-Type" then (
                # TODO maybe check that encoding is utf-8?
            );
        );
        let content_length = content_length
            |> Option.unwrap_or_else(
                () => panic("Content-Length absent")
            );
        { .content_length }
    );

    const write = (io :: Io, message :: Json.t) => (
        let contents = message |> Json.into_dep |> to_string;
        dbg.print(contents);
        let content_length :: Int32 = @native "Buffer.byteLength(\(contents), 'utf-8')";
        io.output.write("Content-Length: ");
        io.output.write(to_string(content_length));
        io.output.write("\r\n\r\n");
        io.output.write(contents);
    );

    const run = (io :: Io, handler :: Handler) => (
        loop (
            let { .content_length } = read_header(io);
            let content = io.input.read_exactly(content_length);
            let json = dep_json.parse(&mut dep_json.Reader.create(&content))
                |> std.Result.unwrap
                |> Json.from_dep;
            let :Object message = json;
            let (&(:String jsonrpc)) = &message |> OrdMap.get("jsonrpc") |> Option.unwrap;
            if jsonrpc != "2.0" then (
                panic("jsonrpc is not 2.0 but " + String.escape(jsonrpc));
            );
            if &message |> OrdMap.get("id") is :Some &id then (
                let result = handler.on_request(json);
                let mut response_fields = OrdMap.new();
                &mut response_fields |> OrdMap.add("id", id);
                match result with (
                    | :Ok result => (
                        &mut response_fields |> OrdMap.add("result", result)
                    )
                    | :Error error_message => (
                        let mut error_fields = OrdMap.new();
                        const INTERNAL_ERROR = -32603 :: Float64;
                        &mut error_fields |> OrdMap.add("code", :Number INTERNAL_ERROR);
                        &mut error_fields |> OrdMap.add("message", :String error_message);
                        &mut response_fields |> OrdMap.add("error", :Object error_fields);
                    )
                );
                let response :: Json.t = :Object response_fields;
                write(io, response);
            ) else (
                handler.on_notification(json);
            );
        );
    );
);
