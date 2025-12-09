module:

const gen_range = [T] (.min :: T, .max :: T) with rng -> T => cfg_if (
    | target.name == "interpreter" => (@native "rng.gen_range") (.min, .max)
);
