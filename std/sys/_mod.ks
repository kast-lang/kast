module:
const chdir :: string -> () = path => cfg_if (
    | target.name == "interpreter" => (@native "sys.chdir") path
);
const argc = () -> int32 => cfg_if (
    | target.name == "interpreter" => (@native "sys.argc") ()
    # kast program.ks hgdhfgdf
);
const argv_at = (idx :: int32) -> string => cfg_if (
    | target.name == "interpreter" => (@native "sys.argv_at") idx
);
# accepts the command to exec, returns the return-code
const exec = (cmd :: string) -> int32 => cfg_if (
    | target.name == "interpreter" => (@native "sys.exec") cmd
)
