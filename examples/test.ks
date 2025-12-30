const std = (
    module:
    
    const String = @native "string";
    
    let print = (s :: String) -> () => @cfg (
        | target.name == "interpreter" => (@native "io.print") s
        | target.name == "javascript" => (@native "console.log") s
    );
    
    const dbg = (
        module:
        
        const print = [T] (x :: T) -> () => @cfg (
            | target.name == "interpreter" => (@native "dbg.print") x
            | target.name == "javascript" => (@native "(x=>console.log([x]))") x
        );
    );
    
    const Option = [T] newtype (
        | :None
        | :Some T
    );
);

use std.*;

let mut message = "hello";
let print_message = () => print message;
print_message ();
message = "world";
print_message ();
let message_ref = &mut message;
message_ref^ = "!!!";
print_message ();

let mut opt :: Option[_] = :Some "HAI";
if opt is :Some message then (
    print message;
);
match opt with (
    | :Some ref mut value => (
        value^ = "HACKED";
    )
    | :None => ()
);
dbg.print opt;
