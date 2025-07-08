module Args : sig
  type args
  type t = args

  val parse : string list -> args
end

val eval : Args.t -> unit
val run : Args.t -> unit
val repl : Args.t -> unit
