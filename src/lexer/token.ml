open Std

type punct = { raw : string }
type ident = { raw : string }
type string_token = { raw : string; contents : string }
type number = { raw : string }
type comment = { raw : string }

type t =
  | Punct of punct
  | Ident of ident
  | String of string_token
  | Number of number
  | Comment of comment
  | Eof

type token = t

let print : Format.formatter -> token -> unit =
 fun fmt token ->
  match token with
  | Punct { raw; _ } -> fprintf fmt "<punct %S>" raw
  | Ident { raw; _ } ->
      if String.exists Char.is_whitespace raw then fprintf fmt "<ident %S>" raw
      else fprintf fmt "<ident %S>" raw
  | Number { raw; _ } -> fprintf fmt "<num %s>" raw
  | String { raw; _ } -> fprintf fmt "<str %s>" raw
  | Comment _ -> fprintf fmt "<comment>"
  | Eof -> fprintf fmt "<eof>"

let raw : token -> string option = function
  | Punct { raw; _ } -> Some raw
  | Ident { raw; _ } -> Some raw
  | String { raw; _ } -> Some raw
  | Number { raw; _ } -> Some raw
  | Comment { raw; _ } -> Some raw
  | Eof -> None

let is_raw : string -> token -> bool =
 fun expected_raw token -> Some expected_raw = raw token

let is_comment : token -> bool =
 fun token -> match token with Comment _ -> true | _ -> false

type string = string_token
