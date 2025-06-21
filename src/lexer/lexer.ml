open Util
open Stdext
module Token = Token
module Reader = Reader

type token = Token.t
type rule = Reader.t -> token option

type lexer = {
  mutable peeked : token spanned option;
  rules : rule list;
  reader : Reader.t;
  source : source;
}

let init : rule list -> source -> lexer =
 fun rules source ->
  { rules; peeked = None; reader = Reader.init source.contents; source }

let peek : lexer -> token spanned =
 fun ({ reader; _ } as lexer) ->
  (if Option.is_none lexer.peeked then
     let try_rule : rule -> token spanned option =
      fun rule ->
       let start = reader.position in
       let* token = rule reader in
       let finish = reader.position in
       Some
         ({
            value = token;
            span = { start; finish; filename = lexer.source.filename };
          }
           : token spanned)
     in
     let token : token spanned =
       match List.find_map try_rule lexer.rules with
       | Some token -> token
       | None -> failwith "no reader could read a token"
     in
     lexer.peeked <- Some token);
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
    failwith
    @@
    (* TODO just sprintf? *)
    (fprintf Format.str_formatter "expected %S, got %a" expected_raw
       (Spanned.print Token.print)
       token;
     Format.flush_str_formatter ())

let default_rules : rule list =
  let read_eof reader =
    match Reader.peek reader with Some _ -> None | None -> Some Token.Eof
  in
  let read_whitespace reader =
    ignore @@ Reader.read_while Char.is_whitespace reader;
    None
  in
  let read_string reader =
    with_return (fun { return } ->
        let* c = Reader.peek reader in
        if c != '\'' && c != '"' then return None;
        let token = Token.String (failwith "todo") in
        Some token)
  in
  let read_ident reader =
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
  let read_punct reader =
    let* c = Reader.peek reader in
    let is_punct : char -> bool =
     fun c -> not (Char.is_whitespace c || Char.is_alphanumberic c || c == '_')
    in
    if is_punct c then
      let token : Token.punct = { raw = Reader.read_while is_punct reader } in
      Some (Token.Punct token)
    else None
  in
  [ read_eof; read_whitespace; read_ident; read_punct; read_string ]

let read_all : rule list -> source -> token spanned list =
 fun rules source ->
  let lexer = init rules source in
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
