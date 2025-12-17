use std.net.tcp;
use std.String;

# connect
let listener = tcp.Listener.bind "127.0.0.1:1234";

# start listening
tcp.Listener.listen (&listener, 5);

# accept one client connection
let client = tcp.Listener.accept (&listener, .close_on_exec = false);
std.io.print <| "Connected to client " + client.addr;

let client_msg = tcp.Stream.read_line &client.stream;
std.io.print <| "Client said:\n" + client_msg;

# write to stream
tcp.Stream.write (
    &client.stream,
    &"Hello from server\n"
);

# close client connection
client.stream |> tcp.Stream.close;

# close server
listener |> tcp.Listener.close;
