module Args : sig
  type args
  type t = args

  val parse : string list -> args
end

val run : Args.t -> unit
