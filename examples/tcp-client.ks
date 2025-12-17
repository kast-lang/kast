use std.net.tcp;
use std.String;

# connect
let stream = tcp.Stream.connect "127.0.0.1:1234";

# write to stream
tcp.Stream.write (
    &stream,
    &"Hello from client\n"
);
std.io.print "Waiting for reply from server";

# read from stream
let response = tcp.Stream.read_line &stream;
std.io.print <| "Server said:\n" + response;
