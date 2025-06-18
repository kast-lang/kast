module:

const now = fn(()) -> float64 {
    cfg_if {
        | target.name == "interpreter" => native "time.now" ()
        | target.name == "javascript" => native "performance.now()/1000"
    }
};
