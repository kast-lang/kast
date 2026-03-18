open Std
open Kast_util

module Types = struct
  type token =
    { shape : shape
    ; span : Span.t
    }

  and punct = { raw : string }

  and ident =
    { raw : string
    ; name : string
    }

  and string_part =
    | Content of
        { raw : string
        ; span : Span.t
        }
    | Interpolate of token list

  and string_shape =
    { raw : string
    ; delimeter : string
    ; parts : string_part list
    ; open_span : Span.t
    ; close_span : Span.t
    }

  and number = { raw : string }

  and comment =
    { raw : string
    ; ty : comment_ty
    }

  and comment_ty =
    | Line
    | Block

  and shape =
    | Punct of punct
    | Ident of ident
    | String of string_shape
    | Number of number
    | Comment of comment
    | Eof
  [@@deriving eq, ord]
end

module Shape = struct
  type t = Types.shape

  let equal = Types.equal_shape
  let compare = Types.compare_shape

  let rec print : formatter -> t -> unit =
    fun fmt shape ->
    match shape with
    | Punct { raw; _ } ->
      fprintf fmt "%a @{<dim;italic><punct>@}" String.print_maybe_escaped raw
    | Ident { raw; _ } -> fprintf fmt "@{<under>%s@} @{<dim;italic><ident>@}" raw
    | Number { raw; _ } -> fprintf fmt "@{<italic>%s@} @{<dim;italic><num>@}" raw
    | String { parts; _ } ->
      parts
      |> List.iteri (fun i (part : Types.string_part) ->
        if i <> 0 then fprintf fmt " ";
        match part with
        | Content { raw; _ } -> fprintf fmt "@{<green>%a@}" String.print_debug raw
        | Interpolate tokens ->
          List.print (fun fmt (token : Types.token) -> print fmt token.shape) fmt tokens)
    | Comment _ -> fprintf fmt "@{<italic><comment>@}"
    | Eof -> fprintf fmt "@{<italic><eof>@}"
  ;;

  let raw : t -> string option = function
    | Punct { raw; _ } -> Some raw
    | Ident { raw; _ } -> Some raw
    | String { raw; _ } -> Some raw
    | Number { raw; _ } -> Some raw
    | Comment { raw; _ } -> Some raw
    | Eof -> None
  ;;

  let is_raw : string -> t -> bool =
    fun expected_raw shape -> Some expected_raw = raw shape
  ;;

  let is_eof : t -> bool = function
    | Eof -> true
    | _ -> false
  ;;

  let is_comment : t -> bool =
    fun shape ->
    match shape with
    | Comment _ -> true
    | _ -> false
  ;;

  let as_float : t -> float = function
    | Number { raw; _ } ->
      (try Float.of_string raw with
       | Failure _ -> invalid_arg "Token.Shape.as_float")
    | _ -> invalid_arg "Token.Shape.as_float"
  ;;

  type string = Types.string_shape
end

type t = Types.token

let equal = Types.equal_token
let compare = Types.compare_token
let raw (token : t) = Shape.raw token.shape
let is_raw (s : string) (token : t) = Shape.is_raw s token.shape
let is_eof (token : t) = Shape.is_eof token.shape
let is_comment (token : t) = Shape.is_comment token.shape

let print fmt ({ shape; span } : t) =
  fprintf fmt "%a @{<dim>at %a@}" Shape.print shape Span.print span
;;

type comment =
  { shape : Types.comment
  ; span : Span.t
  }
[@@deriving eq, ord]
