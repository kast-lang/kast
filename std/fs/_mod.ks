module:
const read_file :: string -> string = path => cfg_if (
    | (@native "==") (target.name, "interpreter") => (@native "fs.read_file") path
);
