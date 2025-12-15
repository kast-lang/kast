module:

const TcpStream = @opaque_type;

const connect = (addr :: String) -> TcpStream => cfg_if (
    | target.name == "interpreter" => (@native "net.tcp.connect") addr
);

const read_line = (stream :: &TcpStream) -> String => cfg_if (
    | target.name == "interpreter" => (@native "net.tcp.read_line") stream
);

const write = (stream :: &TcpStream, data :: &String) -> () => cfg_if (
    | target.name == "interpreter" => (@native "net.tcp.write") (stream, data)
);
