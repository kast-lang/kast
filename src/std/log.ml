open Format

module Log = struct
  type level =
    | Error
    | Warn
    | Info
    | Debug
    | Trace
    | None

  let max_level : level ref = ref Info
  let set_max_level : level -> unit = fun level -> max_level := level

  type 'a log_fn = (('a, formatter, unit) format -> 'a) -> unit

  let with_level : 'a. level -> 'a log_fn -> unit =
    fun level f -> if level <= !max_level then f (fun fmt -> eprintln fmt) else ()
  ;;

  let error : 'a. 'a log_fn -> unit = fun f -> with_level Error f
  let warn : 'a. 'a log_fn -> unit = fun f -> with_level Warn f
  let info : 'a. 'a log_fn -> unit = fun f -> with_level Info f
  let debug : 'a. 'a log_fn -> unit = fun f -> with_level Debug f
  let trace : 'a. 'a log_fn -> unit = fun f -> with_level Trace f
end
