open Std
open Kast_util

module Shape = struct
  type punct = { raw : string }

  and ident =
    { raw : string
    ; name : string
    }

  and string_shape =
    { raw : string
    ; delimeter : string
    ; contents : string
    }

  and number = { raw : string }
  and comment = { raw : string }

  and shape =
    | Punct of punct
    | Ident of ident
    | String of string_shape
    | Number of number
    | Comment of comment
    | Eof
  [@@deriving eq, ord]

  type t = shape

  let equal = equal_shape
  let compare = compare_shape

  let print : formatter -> shape -> unit =
    fun fmt shape ->
    match shape with
    | Punct { raw; _ } ->
      fprintf fmt "%a @{<dim;italic><punct>@}" String.print_maybe_escaped raw
    | Ident { raw; _ } -> fprintf fmt "@{<under>%s@} @{<dim;italic><ident>@}" raw
    | Number { raw; _ } -> fprintf fmt "@{<italic>%s@} @{<dim;italic><num>@}" raw
    | String { raw; _ } -> fprintf fmt "@{<green>%s@} @{<dim;italic><str>@}" raw
    | Comment _ -> fprintf fmt "@{<italic><comment>@}"
    | Eof -> fprintf fmt "@{<italic><eof>@}"
  ;;

  let raw : shape -> string option = function
    | Punct { raw; _ } -> Some raw
    | Ident { raw; _ } -> Some raw
    | String { raw; _ } -> Some raw
    | Number { raw; _ } -> Some raw
    | Comment { raw; _ } -> Some raw
    | Eof -> None
  ;;

  let is_raw : string -> shape -> bool =
    fun expected_raw shape -> Some expected_raw = raw shape
  ;;

  let is_comment : shape -> bool =
    fun shape ->
    match shape with
    | Comment _ -> true
    | _ -> false
  ;;

  let as_float : shape -> float = function
    | Number { raw; _ } ->
      (try Float.of_string raw with
       | Failure _ -> invalid_arg "Token.Shape.as_float")
    | _ -> invalid_arg "Token.Shape.as_float"
  ;;

  type string = string_shape
end

type t =
  { shape : Shape.t
  ; span : Span.t
  }
[@@deriving eq, ord]

type token = t

let raw token = Shape.raw token.shape
let is_raw s token = Shape.is_raw s token.shape

let print fmt { shape; span } =
  fprintf fmt "%a @{<dim>at %a@}" Shape.print shape Span.print span
;;

type comment =
  { shape : Shape.comment
  ; span : Span.t
  }
[@@deriving eq, ord]
