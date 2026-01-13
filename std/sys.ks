module:
const chdir :: String -> () = path => @cfg (
    | target.name == "interpreter" => (@native "sys.chdir")(path)
    | target.name == "javascript" => (@native "Kast.sys.chdir")(path)
);
const argc = () -> Int32 => @cfg (
    | target.name == "interpreter" => (@native "sys.argc")()
    | target.name == "javascript" => (@native "Kast.sys.argc")()
);
const argv_at = (idx :: Int32) -> String => @cfg (
    | target.name == "interpreter" => (@native "sys.argv_at")(idx)
    | target.name == "javascript" => (@native "Kast.sys.argv_at")(idx)
);
# accepts the command to exec, returns the return-code
const exec = (cmd :: String) -> Int32 => @cfg (
    | target.name == "interpreter" => (@native "sys.exec")(cmd)
    | target.name == "javascript" => (@native "Kast.sys.exec")(cmd)
);
# accepts the environment variable, returns its value if it exists or NotFound
const get_env = (var :: String) -> Option.t[String] => @cfg (
    | target.name == "interpreter" => (@native "sys.get_env")(var)
    | target.name == "javascript" => (@native "Kast.sys.get_env")(var)
);
# exit with the provided return code
const exit = [T] (status_code :: Int32) -> T => @cfg (
    | target.name == "interpreter" => (@native "sys.exit")(status_code)
    | target.name == "javascript" => (@native "Kast.sys.exit")(status_code)
);
