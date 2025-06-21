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
  reader : Reader.t;
  source : source;
}

let init : rule list -> source -> lexer =
 fun rules source ->
  {
    rules;
    peeked = None;
    position = Position.beginning;
    reader = Reader.init source.contents;
    source;
  }

let peek : lexer -> token spanned =
 fun ({ reader; _ } as lexer) ->
  if Option.is_none lexer.peeked then (
    let skip_whitespace : unit -> unit =
     fun () -> ignore @@ Reader.read_while Char.is_whitespace reader
    in
    let read_string : unit -> token option =
     fun () ->
      with_return (fun { return } ->
          let* c = Reader.peek reader in
          if c != '\'' && c != '"' then return None;
          let token = Token.String (failwith "todo") in
          Some token)
    in
    let read_ident : unit -> token option =
     fun () ->
      let* c = Reader.peek reader in
      if Char.is_alpha c || c == '_' then
        let ident : Token.ident =
          {
            raw =
              Reader.read_while
                (fun c -> Char.is_alphanumberic c || c == '_')
                reader;
          }
        in
        Some (Token.Ident ident)
      else None
    in
    let read_punct : unit -> token option =
     fun () ->
      let* c = Reader.peek reader in
      let is_punct : char -> bool =
       fun c -> not (Char.is_whitespace c || Char.is_alphanumberic c || c == '_')
      in
      if is_punct c then
        let token : Token.punct = { raw = Reader.read_while is_punct reader } in
        Some (Token.Punct token)
      else None
    in
    let read_eof : unit -> token option =
     fun () ->
      match Reader.peek reader with Some _ -> None | None -> Some Token.Eof
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
