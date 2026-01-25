module:

const GenRange = [Self] newtype {
    .gen :: (.min :: Self, .max :: Self) -> Self,
};

const gen_range = [T] (.min :: T, .max :: T) with rng -> T => (
    (T as GenRange).gen(.min, .max)
);

impl Int32 as GenRange = {
    .gen = (.min, .max) => @cfg (
        | target.name == "interpreter" => (@native "random.gen_range")(.min, .max)
        | target.name == "javascript" => (@native "Math.floor")((@native "Math.random()") * (max - min + 1)) + min
    ),
};

impl Int64 as GenRange = {
    .gen = (.min, .max) => @cfg (
        | target.name == "interpreter" => (@native "random.gen_range")(.min, .max)
        | target.name == "javascript" => (@native "Math.floor")((@native "Math.random()") * (max - min + 1)) + min
    ),
};

impl Float64 as GenRange = {
    .gen = (.min, .max) => @cfg (
        | target.name == "interpreter" => (@native "random.gen_range")(.min, .max)
        | target.name == "javascript" => (@native "Math.random()") * (max - min) + min
    ),
};
