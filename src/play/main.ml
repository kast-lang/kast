open Format

module JSON = struct
  (* Invariants: utf8 strings, unique keys *)
  type t =
    | Null
    | Bool of bool
    | Number of float
    | String of string
    | List of t list
    | Object of (string * t) list

  (** Good-looking, round-trippable floats *)
  let number_to_string n =
    let s = sprintf "%.15g" n in
    if Float.of_string s = n then s else sprintf "%.17g" n

  let pp_string_body ppf =
    String.iter (function
      | '"' -> fprintf ppf {|\"|} (* {|"|} *)
      | '\\' -> fprintf ppf {|\\|}
      | '\b' -> fprintf ppf {|\b|}
      | '\x0C' -> fprintf ppf {|\f|}
      | '\n' -> fprintf ppf {|\n|}
      | '\r' -> fprintf ppf {|\r|}
      | '\t' -> fprintf ppf {|\t|}
      | '\x00' .. '\x1F' as non_print_char ->
          fprintf ppf {|\u%.4X|} (Char.code non_print_char)
      | char -> fprintf ppf {|%c|} char)

  let box pp ppf value = fprintf ppf "@[<v>%a@]" pp value

  let pp_print_list ~sep pp_item =
    Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item

  let rec pp ppf = function
    | Null -> fprintf ppf "null"
    | Bool b -> fprintf ppf "%b" b
    | Number n -> fprintf ppf "%s" (number_to_string n)
    | String s -> fprintf ppf {|"%a"|} pp_string_body s
    | List a ->
        fprintf ppf "[@;<0 2>%a@;<0 0>]"
          (pp_print_list ~sep:",@;<1 2>" (box pp))
          a
    | Object o ->
        fprintf ppf "{@;<0 2>%a@;<0 0>}"
          (pp_print_list ~sep:",@;<1 2>" (box pp_pair))
          o

  and pp_pair ppf (field, value) =
    fprintf ppf {|"%a": %a|} pp_string_body field pp value

  let to_string = asprintf "%a" (box pp)
end

let example : JSON.t =
  Object
    [
      ("name", String "Alice");
      ("age", Number 30.0);
      ("tags", List [ String "ocaml"; String "format" ]);
      ("metadata", Object [ ("id", Number 1.0); ("active", Bool true) ]);
    ]

let () = printf "%s@." (JSON.to_string example)
