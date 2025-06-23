module Format = struct
  include Stdlib.Format

  let boxed : (formatter -> unit) -> formatter -> unit =
   fun f fmt ->
    pp_open_box fmt 2;
    f fmt;
    pp_close_box fmt ()

  module Iter = struct
    type sep = {
      fits : string * int * string;
      breaks : string * int * string;
    }

    type options = {
      before : sep;
      after : sep;
      sep : sep;
    }

    let print_sep { fits; breaks } fmt = pp_print_custom_break fmt ~fits ~breaks

    let print (options : options) iterations print_value (fmt : formatter) iter
        : unit =
      let pp_sep fmt () = print_sep options.sep fmt in
      print_sep options.before fmt;
      pp_print_iter ~pp_sep iterations print_value fmt iter;
      print_sep options.after fmt
  end

  let noop_formatter : formatter =
    make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let comma_separator : formatter -> unit -> unit =
   fun fmt () ->
    pp_print_custom_break fmt ~fits:(",", 1, "") ~breaks:(",", 0, "")

  let trailing_comma : formatter -> unit -> unit =
   fun fmt () -> pp_print_custom_break fmt ~fits:("", 0, "") ~breaks:(",", 0, "")

  let fprintln : 'a. formatter -> ('a, formatter, unit) format -> 'a =
   fun fmt ->
    pp_open_vbox fmt 0;
    kfprintf
      (fun fmt ->
        pp_close_box fmt ();
        fprintf fmt "\n";
        pp_print_flush fmt ())
      fmt

  let sprintln format = fprintln str_formatter format
  let eprintln format = fprintln err_formatter format
  let println format = fprintln std_formatter format

  let make_string : 'a. ('a, formatter, unit, tag) format4 -> 'a =
   fun format -> kfprintf (fun _ -> flush_str_formatter ()) str_formatter format
end

type formatter = Format.formatter

let fprintf = Format.fprintf
let fprintln = Format.fprintln
let println = Format.println
let eprintln = Format.eprintln
let sprintln = Format.sprintln
let make_string = Format.make_string
