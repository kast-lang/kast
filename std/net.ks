module:

const tcp = (
    module:
    
    const Stream = @opaque_type "tcp_Stream";
    
    impl Stream as module = (
        module:

        const connect = async (addr :: String) -> Stream => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.connect")(addr)
            | target.name == "c" => @native "tcp_Stream_connect(\(addr))"
            | target.name == "javascript" => (@native "Kast.net.tcp.connect")(addr)
        );
        
        const read_line = async (stream :: &mut Stream) -> String => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.read_line")(stream)
            | target.name == "c" => @native "tcp_Stream_read_line(\(stream))"
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.read_line")(stream)
        );
        
        const write = async (stream :: &mut Stream, data :: &String) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.write")(stream, data)
            | target.name == "c" => @native "tcp_Stream_write(\(stream), \(data))"
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.write")(stream, data)
        );
        
        const close = async (stream :: Stream) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.stream.close")(stream)
            | target.name == "c" => @native "tcp_Stream_close(\(stream))"
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.close")(stream)
        );
    );
    
    const Listener = @opaque_type "tcp_Listener";
    
    impl Listener as module = (
        module:
        
        const bind = async (addr :: String) -> Listener => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.bind")(addr)
            | target.name == "c" => @native "tcp_Listener_bind(\(addr))"
            | target.name == "javascript" => (@native "Kast.net.tcp.bind")(addr)
        );
        
        const listen = async (listener :: &mut Listener, max_pending :: Int32) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.listen")(listener, max_pending)
            | target.name == "c" => @native "tcp_Listener_listen(\(listener), \(max_pending))"
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.listen")(listener, max_pending)
        );
        
        const accept = async (listener :: &mut Listener, .close_on_exec :: Bool) -> {
            .stream :: Stream,
            .addr :: String
        } => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.accept")(listener, close_on_exec)
            | target.name == "c" => (
                let result :: @opaque_type "tcp_Listener_accepted" = @native "tcp_Listener_accept(\(listener), \(close_on_exec))";
                {
                    .stream = @native "\(result).stream",
                    .addr = @native "\(result).addr",
                }
            )
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.accept")(listener, close_on_exec)
        );
        
        const close = async (listener :: Listener) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.listener.close")(listener)
            | target.name == "c" => @native "tcp_Listener_close(\(listener))"
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.close")(listener)
        );
    );
)
