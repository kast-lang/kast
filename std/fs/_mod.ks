module:
const read_file :: string -> string = path => cfg_if (
    | target.name == "interpreter" => (@native "fs.read_file") path
);
