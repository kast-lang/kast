module:
const gen_int32 = (.min :: int32, .max :: int32) with rng -> int32 => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "rng") (.min, .max)
        | (native "==") (target.name, "ocaml") => (native "Natives.rng") (.min, .max)
    )
)
