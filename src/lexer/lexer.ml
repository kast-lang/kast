open Util
open Stdext
module Token = Token

type token = Token.t

(* TODO *)
type rule = unit

type lexer = {
  mutable peeked : token spanned option;
  mutable position : position;
  rules : rule list;
  source : source;
}

let init : rule list -> source -> lexer =
 fun rules source ->
  { rules; peeked = None; position = Position.beginning; source }

let peek : lexer -> token spanned =
 fun lexer ->
  if Option.is_none lexer.peeked then (
    let peek_char : unit -> char option =
     fun () ->
      let c = String.get lexer.source.contents lexer.position.index in
      c
    in
    let consume_char : unit -> unit =
     fun () ->
      match peek_char () with
      | Some c -> lexer.position <- Position.advance c lexer.position
      | None -> failwith "huh"
    in
    let read_while : (char -> bool) -> string =
     fun predicate ->
      let buffer = Buffer.create 0 in
      let rec loop () =
        match peek_char () with
        | Some c when predicate c ->
            consume_char ();
            Buffer.add_char buffer c;
            loop ()
        | _ -> ()
      in
      loop ();
      Buffer.contents buffer
    in
    let skip_whitespace : unit -> unit =
     fun () -> ignore @@ read_while Char.is_whitespace
    in
    let read_string : unit -> token option =
     fun () ->
      with_return (fun { return } ->
          let* c = peek_char () in
          if c != '\'' && c != '"' then return None;
          let token = Token.String (failwith "todo") in
          Some token)
    in
    let read_ident : unit -> token option =
     fun () ->
      let* c = peek_char () in
      if Char.is_alpha c || c == '_' then
        let ident : Token.ident =
          { raw = read_while (fun c -> Char.is_alphanumberic c || c == '_') }
        in
        Some (Token.Ident ident)
      else None
    in
    let read_punct : unit -> token option =
     fun () ->
      let* c = peek_char () in
      let is_punct : char -> bool =
       fun c -> not (Char.is_whitespace c || Char.is_alphanumberic c || c == '_')
      in
      if is_punct c then
        let token : Token.punct = { raw = read_while is_punct } in
        Some (Token.Punct token)
      else None
    in
    let read_eof : unit -> token option =
     fun () -> match peek_char () with Some _ -> None | None -> Some Token.Eof
    in
    (* actual code *)
    skip_whitespace ();
    let start = lexer.position in
    let possible_token_readers : (unit -> token option) list =
      [ read_eof; read_string; read_ident; read_punct ]
    in
    let token : token =
      match List.find_map (fun f -> f ()) possible_token_readers with
      | Some token -> token
      | None -> failwith "no reader could read a token"
    in
    let finish = lexer.position in
    lexer.peeked <-
      Some
        {
          value = token;
          span = { start; finish; filename = lexer.source.filename };
        };
    ());
  Option.get lexer.peeked

let next : lexer -> token spanned =
 fun lexer ->
  let result = peek lexer in
  lexer.peeked <- None;
  result

let skip : lexer -> unit = fun lexer -> ignore @@ next lexer

let expect_next : lexer -> string -> unit =
 fun lexer expected_raw ->
  let token = peek lexer in
  if Token.is_raw expected_raw token.value then skip lexer
  else
    failwith @@ "expected " ^ expected_raw ^ ", got "
    ^ Spanned.show Token.show token

let read_all : source -> token spanned list =
 fun source ->
  let lexer = init [] source in
  let found_eof = ref false in
  let dispenser : unit -> token spanned option =
   fun () ->
    if !found_eof then None
    else
      let token = next lexer in
      (match token.value with Token.Eof -> found_eof := true | _ -> ());
      Some token
  in
  List.of_seq (Seq.of_dispenser dispenser)
