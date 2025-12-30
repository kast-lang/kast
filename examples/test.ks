const std = (
    module:
    
    const String = @native "string";
    
    let print = (s :: String) -> () => @cfg (
        | target.name == "interpreter" => (@native "io.print") s
        | target.name == "javascript" => (@native "console.log") s
    );
);

use std.*;

print "Hello, World";
