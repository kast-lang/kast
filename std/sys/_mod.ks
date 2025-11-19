module:
const chdir :: string -> () = path => cfg_if (
    | (native "==") (target.name, "interpreter") => (native "sys.chdir") path
);
