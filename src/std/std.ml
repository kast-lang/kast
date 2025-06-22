module Format = struct
  include Format

  let boxed : (formatter -> unit) -> formatter -> unit =
   fun f fmt ->
    pp_open_box fmt 2;
    f fmt;
    pp_close_box fmt ()

  module Iter = struct
    type sep = { fits : string * int * string; breaks : string * int * string }
    type options = { before : sep; after : sep; sep : sep }

    let print_sep { fits; breaks } fmt = pp_print_custom_break fmt ~fits ~breaks

    let print (type iter) (type a) (options : options) iterations print_value
        (fmt : formatter) (iter : iter) : unit =
      let pp_sep fmt () = print_sep options.sep fmt in
      print_sep options.before fmt;
      pp_print_iter ~pp_sep iterations print_value fmt iter;
      print_sep options.after fmt
  end

  let noop_formatter : formatter =
    Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let comma_separator : formatter -> unit -> unit =
   fun fmt () ->
    pp_print_custom_break fmt ~fits:(",", 1, "") ~breaks:(",", 0, "")

  let trailing_comma : formatter -> unit -> unit =
   fun fmt () -> pp_print_custom_break fmt ~fits:("", 0, "") ~breaks:(",", 0, "")

  let fprintln : 'a. formatter -> ('a, formatter, unit) format -> 'a =
   fun fmt ->
    kfprintf
      (fun fmt ->
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

module Char = struct
  include Char

  let is_alpha : char -> bool = function
    | 'A' .. 'Z' -> true
    | 'a' .. 'z' -> true
    | _ -> false

  let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
  let is_alphanumberic : char -> bool = fun c -> is_alpha c || is_digit c

  let is_whitespace : char -> bool =
   fun c -> c == ' ' || c == '\t' || c == '\r' || c == '\n'

  let print : formatter -> char -> unit = fun fmt c -> fprintf fmt "%c" c
end

module String = struct
  include String

  let get : string -> int -> char option =
   fun s i ->
    if 0 <= i && i < String.length s then Some (String.get s i) else None

  let print_dbg : formatter -> string -> unit = fun fmt s -> fprintf fmt "%S" s
end

module Option = struct
  include Option

  let map_or : 'a 'r. 'r -> ('a -> 'r) -> 'a option -> 'r =
   fun default f opt -> match opt with Some x -> f x | None -> default

  let or_else : 'a. (unit -> 'a option) -> 'a option -> 'a option =
   fun f opt -> match opt with Some x -> Some x | None -> f ()

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a option -> unit =
   fun print_value fmt opt ->
    match opt with
    | None -> fprintf fmt "None"
    | Some value -> print_value fmt value
end

let ( let* ) = Option.bind

(* unwrap_or_else *)
let ( let+ ) : 'a. 'a option -> (unit -> 'a) -> 'a =
 fun opt f -> match opt with None -> f () | Some x -> x

let ( let/ ) : 'a. 'a option -> ('a -> unit) -> unit =
 fun opt f -> match opt with None -> () | Some x -> f x

module List = struct
  include List

  let zip : 'a. 'a list -> 'b list -> ('a * 'b) list = combine

  let format_options : Format.Iter.options =
    {
      before = { fits = ("[", 1, ""); breaks = ("[", 2, "") };
      sep = { fits = (",", 1, ""); breaks = (",", 2, "") };
      after = { fits = ("", 1, "]"); breaks = (",", 0, "]") };
    }

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun print_value fmt list ->
    let print_value fmt value = fprintf fmt "@[<v>%a@]" print_value value in
    Format.Iter.print format_options List.iter print_value fmt list

  let tail = List.tl
  let head = List.hd
  let last list = list |> rev |> head
end

module Array = struct
  include Array

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a array -> unit =
   fun print_value fmt array ->
    Array.to_list array |> List.print print_value fmt
end

module Return = struct
  type 'a return_handle = { return : 'never. 'a -> 'never }

  let return_to : 'r 'never. 'r return_handle -> 'r -> 'never =
   fun handle value -> handle.return value

  let with_return (type r) (f : r return_handle -> r) =
    let exception Return of r in
    let handle = { return = (fun value -> raise @@ Return value) } in
    try f handle with Return x -> x
end

include Return

module Ord = struct
  type 'a ord = 'a -> 'a -> int
  type 'a t = 'a ord

  let min : 'a. 'a ord -> 'a -> 'a -> 'a =
   fun ord a b -> if ord a b < 0 then a else b

  let max : 'a. 'a ord -> 'a -> 'a -> 'a =
   fun ord a b -> if ord b a < 0 then a else b
end

module Range = struct
  module Inclusive = struct
    type 'a t = { min : 'a; max : 'a }

    let unite : 'a. 'a Ord.t -> 'a t -> 'a t -> 'a t =
     fun ord a b ->
      { min = Ord.min ord a.min b.min; max = Ord.max ord a.max b.max }

    let point : 'a. 'a -> 'a t = fun x -> { min = x; max = x }
  end

  type 'a inclusive = 'a Inclusive.t
end

let unreachable : 'never. string -> 'never =
 fun s -> failwith @@ "unreachable reached: " ^ s

module Set = struct
  include Set

  module Make (T : OrderedType) = struct
    include Set.Make (T)

    let contains : elt -> t -> bool =
     fun elem set -> find_opt elem set |> Option.is_some
  end
end

module Log = struct
  type level = Error | Warn | Info | Debug | Trace

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
