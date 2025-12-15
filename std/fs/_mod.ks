module:
const read_file :: String -> String = path => cfg_if (
    | target.name == "interpreter" => (@native "fs.read_file") path
);
