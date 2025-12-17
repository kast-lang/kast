module:
const read_file :: String -> String = path => @cfg (
    | target.name == "interpreter" => (@native "fs.read_file") path
);
