module:
const print = [T] (value :: T) -> () => @cfg (
    | target.name == "interpreter" => (@native "dbg.print") value
    | target.name == "javascript" => (@native "Kast.dbg.print") value
);
