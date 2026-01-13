use std.net.tcp;
use std.String;

# connect
let mut stream = tcp.Stream.connect("127.0.0.1:1234");

# write to stream
tcp.Stream.write(
    &mut stream,
    &"Hello from client\n"
);
std.io.print("Waiting for reply from server");

# read from stream
let response = tcp.Stream.read_line(&mut stream);
std.io.print <| "Server said:\n" + response;
