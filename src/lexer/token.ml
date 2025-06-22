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
  | Punct { raw; _ }
  | Ident { raw; _ }
  | String { raw; _ }
  | Number { raw; _ }
  | Comment { raw; _ } ->
      Format.fprintf fmt "%S" raw
  | Eof -> Format.fprintf fmt "<eof>"

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
