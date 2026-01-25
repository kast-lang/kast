module:

const tcp = (
    module:
    
    const Stream = @opaque_type;
    
    impl Stream as module = (
        module:
        const connect = (addr :: String) -> Stream => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.connect")(addr)
            | target.name == "javascript" => (@native "Kast.net.tcp.connect")(addr)
        );
        
        const read_line = (stream :: &mut Stream) -> String => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.read_line")(stream)
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.read_line")(stream)
        );
        
        const write = (stream :: &mut Stream, data :: &String) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.write")(stream, data)
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.write")(stream, data)
        );
        
        const close = (stream :: Stream) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.stream.close")(stream)
            | target.name == "javascript" => (@native "Kast.net.tcp.stream.close")(stream)
        );
    );
    
    const Listener = @opaque_type;
    
    impl Listener as module = (
        module:
        
        const bind = (addr :: String) -> Listener => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.bind")(addr)
            | target.name == "javascript" => (@native "Kast.net.tcp.bind")(addr)
        );
        
        const listen = (listener :: &mut Listener, max_pending :: Int32) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.listen")(listener, max_pending)
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.listen")(listener, max_pending)
        );
        
        const accept = (listener :: &mut Listener, .close_on_exec :: Bool) -> {
            .stream :: Stream,
            .addr :: String
        } => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.accept")(listener, close_on_exec)
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.accept")(listener, close_on_exec)
        );
        
        const close = (listener :: Listener) -> () => @cfg (
            | target.name == "interpreter" => (@native "net.tcp.listener.close")(listener)
            | target.name == "javascript" => (@native "Kast.net.tcp.listener.close")(listener)
        );
    );
)
