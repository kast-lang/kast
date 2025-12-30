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
);

use std.*;

let s = "Hello, World!";
print s;
dbg.print s;
