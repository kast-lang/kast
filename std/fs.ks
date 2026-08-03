module:
const read_file :: async String -> String = path => @cfg (
    | target.name == "interpreter" => (@native "fs.read_file")(path)
    | target.name == "c" => (
        @native "read_file" :: fn @call "C" String -> String
    )(path)
    | target.name == "javascript" => (@native "Kast.fs.read_file")(path)
);
