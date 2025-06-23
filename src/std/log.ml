open Format

module Log = struct
  type level =
    | Error
    | Warn
    | Info
    | Debug
    | Trace

  let max_level : level ref = ref Info
  let set_max_level : level -> unit = fun level -> max_level := level

  let with_level : 'a. level -> ('a, formatter, unit) format -> 'a =
   fun level ->
    if level <= !max_level then eprintln else fprintf Format.noop_formatter

  let error : 'a. ('a, formatter, unit) format -> 'a =
   fun format -> with_level Error format

  let warn : 'a. ('a, formatter, unit) format -> 'a =
   fun format -> with_level Warn format

  let info : 'a. ('a, formatter, unit) format -> 'a =
   fun format -> with_level Info format

  let debug : 'a. ('a, formatter, unit) format -> 'a =
   fun format -> with_level Debug format

  let trace : 'a. ('a, formatter, unit) format -> 'a =
   fun format -> with_level Trace format
end
