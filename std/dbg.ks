module:
const print = [T] (value :: T) -> () => (@native "dbg.print") value;
