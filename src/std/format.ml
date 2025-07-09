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

  let make_string : 'a. ('a, formatter, unit, string) format4 -> 'a =
   fun format ->
    let buffer = Buffer.create 32 in
    let fmt = formatter_of_buffer buffer in
    kfprintf
      (fun _ ->
        pp_print_flush fmt ();
        buffer |> Buffer.contents)
      fmt format

  let ansi_escape : string -> string * string = function
    | "bold" -> ("1", "22")
    | "dim" -> ("2", "22")
    | "italic" -> ("3", "23")
    | "under" -> ("4", "24")
    | "blink" -> ("5", "25")
    | "strike" -> ("9", "29")
    | "black" -> ("30", "39")
    | "red" -> ("31", "39")
    | "green" -> ("32", "39")
    | "yellow" -> ("33", "39")
    | "blue" -> ("34", "39")
    | "magenta" -> ("35", "39")
    | "cyan" -> ("36", "39")
    | "white" -> ("37", "39")
    | "gray" -> ("90", "39")
    | "black_bg" -> ("40", "49")
    | "red_bg" -> ("41", "49")
    | "green_bg" -> ("42", "49")
    | "yellow_bg" -> ("43", "49")
    | "blue_bg" -> ("44", "49")
    | "magenta_bg" -> ("45", "49")
    | "cyan_bg" -> ("46", "49")
    | "white_bg" -> ("47", "49")
    | "gray_bg" -> ("100", "49")
    | name -> invalid_arg @@ "ansi_escape " ^ name
  ;;

  let tag_functions ~tty : formatter_stag_functions =
    if tty then
      let mark_stag which tag =
        match tag with
        | String_tag tag ->
            let codes =
              Stdlib.String.split_on_char ';' tag
              |> Stdlib.List.map (fun tag -> ansi_escape tag |> which)
            in
            "\x1b[" ^ Stdlib.String.concat ";" codes ^ "m"
        | _ -> ""
      in
      {
        mark_open_stag = mark_stag fst;
        mark_close_stag = mark_stag snd;
        print_open_stag = ignore;
        print_close_stag = ignore;
      }
    else
      {
        mark_open_stag = (fun _ -> "");
        mark_close_stag = (fun _ -> "");
        print_open_stag = ignore;
        print_close_stag = ignore;
      }
  in
  pp_set_formatter_stag_functions std_formatter
    (tag_functions ~tty:(Unix.isatty @@ Unix.descr_of_out_channel stdout));
  pp_set_formatter_stag_functions err_formatter
    (tag_functions ~tty:(Unix.isatty @@ Unix.descr_of_out_channel stderr));
  pp_set_tags std_formatter true;
  pp_set_tags err_formatter true
end

type formatter = Format.formatter

let fprintf = Format.fprintf
let fprintln = Format.fprintln
let println = Format.println
let eprintln = Format.eprintln
let sprintln = Format.sprintln
let make_string = Format.make_string
