open Std
open Kast_util

type _ Effect.t += Import : span * Uri.t -> Ruleset.t Effect.t
