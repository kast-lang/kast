type punct = { raw : string }
type ident = { raw : string }
type string_token = { raw : string }
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

let show : token -> string = function
  | Punct { raw; _ } -> raw
  | Ident { raw; _ } -> raw
  | String { raw; _ } -> raw
  | Number { raw; _ } -> raw
  | Comment { raw; _ } -> raw
  | Eof -> "<eof>"

let print : Format.formatter -> token -> unit =
 fun fmt token -> Format.fprintf fmt "%S" (show token)

let raw : token -> string option = function
  | Punct { raw; _ } -> Some raw
  | Ident { raw; _ } -> Some raw
  | String { raw; _ } -> Some raw
  | Number { raw; _ } -> Some raw
  | Comment { raw; _ } -> Some raw
  | Eof -> None

let is_raw : string -> token -> bool =
 fun expected_raw token -> Some expected_raw = raw token

type string = string_token
