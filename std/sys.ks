module:
const chdir :: String -> () = path => @cfg (
    | target.name == "interpreter" => (@native "sys.chdir") path
);
const argc = () -> Int32 => @cfg (
    | target.name == "interpreter" => (@native "sys.argc") ()
    # kast program.ks hgdhfgdf
);
const argv_at = (idx :: Int32) -> String => @cfg (
    | target.name == "interpreter" => (@native "sys.argv_at") idx
);
# accepts the command to exec, returns the return-code
const exec = (cmd :: String) -> Int32 => @cfg (
    | target.name == "interpreter" => (@native "sys.exec") cmd
);
# accepts the environment variable, returns its value if it exists or NotFound
const get_env = (var :: String) -> Option.t[String] => @cfg (
    | target.name == "interpreter" => (@native "sys.get_env") var
);
# exit with the provided return code
const exit = [T] (status_code :: Int32) -> T => @cfg (
    | target.name == "interpreter" => (@native "sys.exit") status_code
);
