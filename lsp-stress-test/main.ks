dbg.print(__FILE__);
dbg.print(std.path.dirname(__FILE__));
std.sys.chdir(std.path.dirname(__FILE__));

let initialize = std.fs.read_file("initialize.json");
let did_open = std.fs.read_file("did_open.json");
let did_change = std.fs.read_file("did_change.json");

const write_message = (content :: String) => (
    let content_length :: Int32 = @native "Buffer.byteLength(\(content), 'utf-8')";
    std.io.stdout.write("Content-Length: ");
    std.io.stdout.write(to_string(content_length));
    std.io.stdout.write("\r\n\r\n");
    std.io.stdout.write(content);
);

const get_time = () -> Float64 => (
    @native "performance.now()"
);

const sleep = (.millis :: Float64) => (
    # @native "await new Promise(resolve => setTimeout(resolve, \(millis)))"
    let start = get_time();
    let finish = start + millis / 1000;
    while get_time() < finish do ();
);

write_message(initialize);
write_message(did_open);

let mut idx = 0;
loop (
    write_message(did_change);
    sleep(.millis = 50);
    eprint("Sent did_change notification #" + to_string(idx));
    idx += 1;
);
