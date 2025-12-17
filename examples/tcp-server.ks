use std.net.tcp;
use std.String;

# connect
let stream = tcp.TcpStream.bind "127.0.0.1:1234";

# start listening
tcp.TcpStream.listen (&stream, 5);

# accept one client connection
let client = tcp.TcpStream.accept (&stream, .close_on_exec = false);
std.io.print <| "Connected to client " + client.addr;

let client_msg = tcp.TcpStream.read_line &client.stream;
std.io.print <| "Client said:\n" + client_msg;

# write to stream
tcp.TcpStream.write (
    &stream,
    &"Hello from server\n"
);

# close client connection
tcp.TcpStream.close client.stream;

# close server
tcp.TcpStream.close stream;
