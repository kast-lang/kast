module:

const gen_range = [T] (.min :: T, .max :: T) with rng -> T => @cfg (
    | target.name == "interpreter" => (@native "rng.gen_range") (.min, .max)
);
