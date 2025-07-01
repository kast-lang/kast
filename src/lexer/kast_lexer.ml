open Std
open Kast_util
module Token = Kast_token
module Reader = Reader

exception Error of (formatter -> unit)

let () =
  Printexc.register_printer (function
    | Error f ->
        eprintln "@{<red>Lexer error:@} %a" (fun fmt () -> f fmt) ();
        exit 1
    | _ -> None)

let error : 'never. ('a, formatter, unit, 'never) format4 -> 'a =
 fun format -> Format.kdprintf (fun f -> raise <| Error f) format

type rule = Reader.t -> Token.Shape.t option

module Recording = struct
  type t = { mutable tokens_rev : Token.t list }

  let equal a b = a == b
  let hash = Hashtbl.hash
end

module RecordingTable = Hashtbl.Make (Recording)

type lexer = {
  mutable peeked : Token.t option;
  rules : rule list;
  reader : Reader.t;
  source : source;
  recordings : unit RecordingTable.t;
}

type t = lexer

let source lexer = lexer.source

let init : rule list -> source -> lexer =
 fun rules source ->
  {
    rules;
    peeked = None;
    reader = Reader.init source.contents;
    source;
    recordings = RecordingTable.create 0;
  }

(* TODO maybe not correct? maybe should be position of peeked token *)
let position lexer = lexer.reader.position

let peek : lexer -> Token.t =
 fun ({ reader; _ } as lexer) ->
  (if Option.is_none lexer.peeked then
     let try_rule : rule -> Token.t option =
      fun rule ->
       let start = reader.position in
       let* shape = rule reader in
       let finish = reader.position in
       Some
         ({ shape; span = { start; finish; filename = lexer.source.filename } }
           : Token.t)
     in
     let token : Token.t =
       match List.find_map try_rule lexer.rules with
       | Some token -> token
       | None ->
           error "Unexpected char @{<green>%C@} @{<dim>at %a@}"
             (Reader.peek reader |> Option.get)
             Position.print reader.position
     in
     lexer.peeked <- Some token);
  Option.get lexer.peeked

let next : lexer -> Token.t =
 fun lexer ->
  let result = peek lexer in
  lexer.recordings
  |> RecordingTable.iter (fun recording () ->
         recording.tokens_rev <- result :: recording.tokens_rev);
  lexer.peeked <- None;
  result

let advance : lexer -> unit = fun lexer -> ignore <| next lexer

let expect_next : lexer -> string -> unit =
 fun lexer expected_raw ->
  let token = peek lexer in
  if Token.is_raw expected_raw token then advance lexer
  else error "expected @{<white>%S@}, got %a" expected_raw Token.print token

let expect_eof : lexer -> unit =
 fun lexer ->
  let peek = peek lexer in
  match peek.shape with
  | Eof -> ()
  | _ -> error "Expected %a, got %a" Token.Shape.print Eof Token.print peek

let default_rules : rule list =
  let read_eof reader =
    match Reader.peek reader with
    | Some _ -> None
    | None -> Some Token.Shape.Eof
  in
  let read_whitespace reader =
    ignore <| Reader.read_while Char.is_whitespace reader;
    None
  in
  let read_string reader =
    with_return (fun { return } ->
        let* c = Reader.peek reader in
        if c != '\'' && c != '"' then return None;
        let delimeter = c in
        let raw = Reader.start_rec reader in
        Reader.advance reader;
        let contents = Buffer.create 0 in
        let rec loop =
         fun () ->
          let/ c = Reader.peek reader in
          if c = delimeter then ()
          else (
            Reader.advance reader;
            let c =
              if c = '\\' then (
                let c =
                  match Reader.peek reader with
                  | Some c -> c
                  | None ->
                      error
                        "Expected escaped char, got @{<italic><eof>@} @{<dim>at %a@}"
                        Position.print reader.position
                in
                let result =
                  match c with
                  | '\\' -> '\\'
                  | 'n' -> '\n'
                  | 't' -> '\t'
                  | _ ->
                      error
                        "Incorrect escape charater %C @{<italic><eof>@} @{<dim>at %a@}"
                        c Position.print reader.position
                in
                Reader.advance reader;
                result)
              else c
            in
            Buffer.add_char contents c;
            loop ())
        in
        loop ();
        (match Reader.peek reader with
        | Some c when c = delimeter -> Reader.advance reader
        | Some c ->
            error "Expected %C, got %C @{<dim>at %a@}" delimeter c
              Position.print reader.position
        | None ->
            error "Expected %C, got @{<italic><eof>@} @{<dim>at %a@}" delimeter
              Position.print reader.position);
        let token : Token.Shape.string =
          { raw = Reader.finish_rec raw; contents = Buffer.contents contents }
        in
        Some (Token.Shape.String token))
  in
  let read_ident reader =
    let* c = Reader.peek reader in
    if Char.is_alpha c || c = '_' then
      let raw =
        Reader.read_while (fun c -> Char.is_alphanumeric c || c = '_') reader
      in
      let ident : Token.Shape.ident = { raw; name = raw } in
      Some (Token.Shape.Ident ident)
    else None
  in
  let read_number reader =
    let* c = Reader.peek reader in
    let* () = if Char.is_digit c then Some () else None in
    let seen_dot = ref false in
    let raw =
      reader
      |> Reader.read_while (function
           | '.' when not !seen_dot ->
               seen_dot := true;
               true
           | c when Char.is_digit c -> true
           | _ -> false)
    in
    Some (Token.Shape.Number { raw })
  in
  let read_punct reader =
    let* c = Reader.peek reader in
    let is_punct : char -> bool = function
      | '_' | '\'' | '"' -> false
      | c when Char.is_whitespace c -> false
      | c when Char.is_alphanumeric c -> false
      | _ -> true
    in
    let is_single_punct = function
      | c when String.contains "(){}[]&^$;" c -> true
      | _ -> false
    in
    if is_punct c then (
      Reader.advance reader;
      let raw =
        match is_single_punct c with
        | true -> String.make 1 c
        | false ->
            String.make 1 c
            ^ Reader.read_while
                (fun c -> is_punct c && not (is_single_punct c))
                reader
      in
      let token : Token.Shape.punct = { raw } in
      Some (Token.Shape.Punct token))
    else None
  in
  let read_line_comment reader =
    let* c = Reader.peek reader in
    let* () = if c = '#' then Some () else None in
    let raw = reader |> Reader.read_while (fun c -> c <> '\n') in
    Some (Token.Shape.Comment { raw })
  in
  let read_block_comment reader =
    let* () = if Reader.peek reader = Some '(' then Some () else None in
    let* () = if Reader.peek2 reader = Some '#' then Some () else None in
    let raw = Reader.start_rec reader in
    let start_pos = reader.position in
    Reader.advance reader;
    Reader.advance reader;
    let nestiness = ref 1 in
    while !nestiness > 0 do
      if Reader.peek reader = Some '#' && Reader.peek2 reader = Some ')' then (
        Reader.advance reader;
        Reader.advance reader;
        nestiness := !nestiness - 1)
      else if Reader.peek reader = Some '(' && Reader.peek2 reader = Some '#'
      then (
        Reader.advance reader;
        Reader.advance reader;
        nestiness := !nestiness + 1)
      else if Reader.peek reader = None then
        error "Unclosed block comment (start at %a) @{<dim>at %a@}"
          Position.print start_pos Position.print reader.position
      else Reader.advance reader
    done;
    Some (Token.Shape.Comment { raw = Reader.finish_rec raw })
  in
  [
    read_whitespace;
    read_eof;
    read_ident;
    read_number;
    read_string;
    read_line_comment;
    read_block_comment;
    read_punct;
  ]

let read_all : rule list -> source -> Token.t list =
 fun rules source ->
  let lexer = init rules source in
  let found_eof = ref false in
  let dispenser : unit -> Token.t option =
   fun () ->
    if !found_eof then None
    else
      let token = next lexer in
      (match token.shape with
      | Token.Shape.Eof -> found_eof := true
      | _ -> ());
      Some token
  in
  List.of_seq (Seq.of_dispenser dispenser)

type recording = {
  recording : Recording.t;
  lexer : lexer;
}

let start_rec : lexer -> recording =
 fun lexer ->
  let recording : Recording.t = { tokens_rev = [] } in
  RecordingTable.add lexer.recordings recording ();
  { recording; lexer }

let stop_rec : recording -> Token.t list =
 fun { recording; lexer } ->
  RecordingTable.remove lexer.recordings recording;
  recording.tokens_rev |> List.rev
