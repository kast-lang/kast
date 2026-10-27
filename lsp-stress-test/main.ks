dbg.print(__FILE__);
dbg.print(std.path.dirname(__FILE__));
std.sys.chdir(std.path.dirname(__FILE__));

let initialize = std.fs.read_file("initialize.json");
let did_open = std.fs.read_file("did_open.json");
let did_change = std.fs.read_file("did_change.json");

const write_message = (content :: &str) => (
    let content_length = String.utf8_length(content);
    std.io.stdout.write("Content-Length: ");
    std.io.stdout.write(&to_string(content_length) |> as_str);
    std.io.stdout.write("\r\n\r\n");
    std.io.stdout.write(content);
);

const sleep = (.millis :: Float64) => (
    let millis :: Int64 = @native "\(millis)";
    std.thread.sleep_ns(millis * 1000);
);

write_message(&initialize |> as_str);
write_message(&did_open |> as_str);

let max_idx :: Option.t[Int32] = if std.sys.argc() >= 2 then (
    :Some parse(std.sys.argv_at(1))
) else (
    :None
);

let mut idx = 0;
loop (
    if max_idx is :Some max then (
        if idx >= max then break;
    );
    write_message(&did_change |> as_str);
    sleep(.millis = 50);
    eprintln!("Sent did_change notification #\(idx)");
    idx += 1;
);
