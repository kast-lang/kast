module:
const read_file :: async &str -> String = path => @cfg (
    | target.name == "interpreter" => (@native "fs.read_file")(path)
    | target.name == "c" => @native "Kast_read_file(\(path))"
    | target.name == "javascript" => (@native "Kast.fs.read_file")(path)
);
