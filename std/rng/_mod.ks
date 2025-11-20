module:
const gen_int32 = (.min :: int32, .max :: int32) with rng -> int32 => (
    cfg_if (
        | target.name == "interpreter" => (@native "rng") (.min, .max)
        | target.name == "ocaml" => (@native "Natives.rng") (.min, .max)
    )
)
