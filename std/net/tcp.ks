module:

# A TCP stream file descriptor
const TcpStream = @opaque_type;

impl TcpStream as module = (
    module:
    const connect = (addr :: String) -> TcpStream => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.connect") addr
    );
    
    const read_line = (stream :: &TcpStream) -> String => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.read_line") stream
    );
    
    const write = (stream :: &TcpStream, data :: &String) -> () => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.write") (stream, data)
    );
    
    const bind = (addr :: String) -> TcpStream => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.bind") addr
    );
    
    const listen = (stream :: &TcpStream, max_pending :: Int32) -> () => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.listen") (stream, max_pending)
    );
    
    const accept = (stream :: &TcpStream, .close_on_exec :: Bool) -> (
        .stream :: TcpStream,
        .addr :: String
    ) => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.accept") (stream, close_on_exec)
    );
    
    const close = (stream :: TcpStream) -> () => cfg_if (
        | target.name == "interpreter" => (@native "net.tcp.close") stream
    );
);
