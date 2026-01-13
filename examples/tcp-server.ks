use std.net.tcp;
use std.String;

# connect
let mut listener = tcp.Listener.bind("127.0.0.1:1234");

# start listening
tcp.Listener.listen(&mut listener, 5);

# accept one client connection
let mut client = tcp.Listener.accept(&mut listener, .close_on_exec = false);
std.io.print <| "Connected to client " + client.addr;

let client_msg = tcp.Stream.read_line(&mut client.stream);
std.io.print <| "Client said:\n" + client_msg;

# write to stream
tcp.Stream.write(&mut client.stream,
&"Hello from server\n");

# close client connection
client.stream |> tcp.Stream.close;

# close server
listener |> tcp.Listener.close;
