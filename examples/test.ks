const String = @native "string";

let print = (s :: String) -> () => (
    (@native "console.log") s;
);

print "Hello, World";
