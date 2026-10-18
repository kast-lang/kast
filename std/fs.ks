module:

# Read a UTF-8 file into a String
const read_file :: async &str -> String = path => @cfg (
    | target.name == "interpreter" => (@native "fs.read_file")(path)
    | target.name == "c" => @native "Kast_read_file(\(path))"
    | target.name == "javascript" => (@native "Kast.fs.read_file")(path)
);

# Read a file into an Array of bytes
const read_file_bytes :: async &str -> std.collections.ArrayList.t[Byte] = path => @cfg (
    | target.name == "interpreter" => (@native "fs.read_file_bytes")(path)
    | target.name == "c" => @native "Kast_read_file_bytes(\(path))"
    | target.name == "javascript" => (@native "Kast.fs.read_file_bytes")(path)
);
