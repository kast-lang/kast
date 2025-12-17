use std.net.tcp;
use std.String;

# connect
let stream = tcp.TcpStream.connect "127.0.0.1:1234";

# write to stream
tcp.TcpStream.write (
    &stream,
    &"Hello from Kast\n"
);
std.io.print "Waiting for reply from server";

# read from stream
let response = tcp.TcpStream.read_line &stream;
std.io.print <| "Server said:\n" + response;
