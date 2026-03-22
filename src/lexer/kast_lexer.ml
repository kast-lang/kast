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
;;

let error : 'never. ('a, formatter, unit, 'never) format4 -> 'a =
  fun format -> Format.kdprintf (fun f -> raise <| Error f) format
;;

module Recording = struct
  type t = { mutable tokens_rev : Token.t list }

  let equal a b = a == b
  let hash = Hashtbl.hash
end

module RecordingTable = Hashtbl.Make (Recording)

type lexer =
  { peeked : Token.t Queue.t
  ; rules : rule list
  ; reader : Reader.t
  ; source : source
  ; recordings : unit RecordingTable.t
  }

and rule = lexer -> Token.Shape.t option

type t = lexer

let source lexer = lexer.source

let init : rule list -> source -> lexer =
  fun rules source ->
  { rules
  ; peeked = Queue.create ()
  ; reader = Reader.init source.contents
  ; source
  ; recordings = RecordingTable.create 0
  }
;;

let init_with : rule list -> Token.t list -> Uri.t -> lexer =
  fun rules tokens uri ->
  let source : source = { contents = ""; uri } in
  { rules
  ; peeked = Queue.of_seq (List.to_seq tokens)
  ; reader = Reader.init source.contents
  ; source
  ; recordings = RecordingTable.create 0
  }
;;

let position lexer =
  match lexer.peeked |> Queue.peek_opt with
  | None -> lexer.reader.position
  | Some token -> token.span.start
;;

let peek : lexer -> Token.t =
  fun ({ reader; _ } as lexer) ->
  if lexer.peeked |> Queue.is_empty
  then (
    let try_rule : rule -> Token.t option =
      fun rule ->
      let start = reader.position in
      let* shape = rule lexer in
      let finish = reader.position in
      Some ({ shape; span = { start; finish; uri = lexer.source.uri } } : Token.t)
    in
    let token : Token.t =
      match List.find_map try_rule lexer.rules with
      | Some token -> token
      | None ->
        error
          "Unexpected char @{<green>%C@} @{<dim>at %a@}"
          (Reader.peek reader |> Option.get)
          Position.print
          reader.position
    in
    lexer.peeked |> Queue.push token);
  lexer.peeked |> Queue.peek
;;

let next : lexer -> Token.t =
  fun lexer ->
  let result = peek lexer in
  lexer.recordings
  |> RecordingTable.iter (fun recording () ->
    recording.tokens_rev <- result :: recording.tokens_rev);
  ignore <| (lexer.peeked |> Queue.pop);
  result
;;

let advance : lexer -> unit = fun lexer -> ignore <| next lexer

let expect_next : lexer -> string -> unit =
  fun lexer expected_raw ->
  let token = peek lexer in
  if Token.is_raw expected_raw token
  then advance lexer
  else
    error
      "expected @{<white>%a@}, got %a"
      String.print_debug
      expected_raw
      Token.print
      token
;;

let expect_eof : lexer -> unit =
  fun lexer ->
  let peek = peek lexer in
  match peek.shape with
  | Eof -> ()
  | _ -> error "Expected %a, got %a" Token.Shape.print Eof Token.print peek
;;

module DefaultRules = struct
  let read_eof lexer =
    match Reader.peek lexer.reader with
    | Some _ -> None
    | None -> Some Token.Types.Eof
  ;;

  let read_whitespace lexer =
    ignore <| Reader.read_while Char.is_whitespace lexer.reader;
    None
  ;;

  let read_string_impl lexer : Token.Shape.string =
    let c = Reader.peek lexer.reader |> Option.get in
    let delimeter = c in
    let open_span = Span.single_char lexer.reader.position lexer.source.uri in
    let raw = Reader.start_rec lexer.reader in
    Reader.advance lexer.reader;
    let contents_raw = ref (Reader.start_rec lexer.reader) in
    let contents = Buffer.create 0 in
    let contents_start = ref lexer.reader.position in
    let parts = ref [] in
    let finish_contents_part () =
      if Buffer.length contents <> 0
      then
        parts
        := !parts
           @ [ Token.Types.Content
                 { raw = Reader.finish_rec !contents_raw
                 ; contents = Buffer.contents contents
                 ; span =
                     { start = !contents_start
                     ; finish = lexer.reader.position
                     ; uri = lexer.source.uri
                     }
                 }
             ];
      contents_raw := Reader.start_rec lexer.reader;
      contents_start := lexer.reader.position;
      Buffer.clear contents
    in
    let rec loop =
      fun () ->
      let/ c = Reader.peek lexer.reader in
      if c = delimeter
      then ()
      else (
        Reader.advance lexer.reader;
        with_return (fun { return } ->
          let c =
            if c = '\\'
            then (
              let c =
                match Reader.peek lexer.reader with
                | Some c -> c
                | None ->
                  error
                    "Expected escaped char, got @{<italic><eof>@} @{<dim>at %a@}"
                    Position.print
                    lexer.reader.position
              in
              let result =
                match c with
                | '(' ->
                  finish_contents_part ();
                  Reader.advance lexer.reader;
                  let tokens = ref [] in
                  while true do
                    ignore (read_whitespace lexer);
                    if Reader.peek lexer.reader = Some ')'
                    then (
                      Reader.advance lexer.reader;
                      parts := !parts @ [ Token.Types.Interpolate !tokens ];
                      contents_raw := Reader.start_rec lexer.reader;
                      return ())
                    else (
                      let next_token = next lexer in
                      if next_token.shape |> Token.Shape.is_eof
                      then
                        error
                          "Unclosed string interpolation, got %a"
                          Token.print
                          next_token;
                      tokens := !tokens @ [ next_token ])
                  done
                | '\\' -> '\\'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 'b' -> '\b'
                | 't' -> '\t'
                | '\'' -> '\''
                | '"' -> '"'
                | 'x' ->
                  Reader.advance lexer.reader;
                  let c1 =
                    match Reader.peek lexer.reader with
                    | Some c ->
                      Reader.advance lexer.reader;
                      c
                    | None ->
                      error
                        "Expected two hex digits, got @{<italic><eof>@} @{<dim>at %a@}"
                        Position.print
                        lexer.reader.position
                  in
                  let c2 =
                    match Reader.peek lexer.reader with
                    | Some c ->
                      Reader.advance lexer.reader;
                      c
                    | None ->
                      error
                        "Expected two hex digits, got @{<italic><eof>@} @{<dim>at %a@}"
                        Position.print
                        lexer.reader.position
                  in
                  if not (Char.is_hex_digit c1 && Char.is_hex_digit c2)
                  then
                    error
                      "Expected two hex digits, got %C and %C @{<dim>at %a@}"
                      c1
                      c2
                      Position.print
                      lexer.reader.position;
                  let s = make_string "0x%c%c" c1 c2 in
                  Buffer.add_char contents (Char.chr (int_of_string s));
                  return ()
                | _ ->
                  error
                    "Incorrect escape charater %C @{<dim>at %a@}"
                    c
                    Position.print
                    lexer.reader.position
              in
              Reader.advance lexer.reader;
              result)
            else c
          in
          Buffer.add_char contents c);
        loop ())
    in
    loop ();
    finish_contents_part ();
    let parts =
      match !parts with
      | [] ->
        [ Token.Types.Content
            { raw = ""
            ; contents = ""
            ; span =
                { start = !contents_start
                ; finish = lexer.reader.position
                ; uri = lexer.source.uri
                }
            }
        ]
      | parts -> parts
    in
    let close_span = Span.single_char lexer.reader.position lexer.source.uri in
    (match Reader.peek lexer.reader with
     | Some c when c = delimeter -> Reader.advance lexer.reader
     | Some c ->
       error
         "Expected %C, got %C @{<dim>at %a@}"
         delimeter
         c
         Position.print
         lexer.reader.position
     | None ->
       error
         "Expected %C, got @{<italic><eof>@} @{<dim>at %a@}"
         delimeter
         Position.print
         lexer.reader.position);
    { raw = Reader.finish_rec raw
    ; parts
    ; delimeter = String.make 1 delimeter
    ; open_span
    ; close_span
    }
  ;;

  let read_string lexer =
    with_return (fun { return } ->
      let* c = Reader.peek lexer.reader in
      if c != '\'' && c != '"' then return None;
      let token = read_string_impl lexer in
      Some (Token.Types.String token))
  ;;

  let is_ident_start (c : char) : bool = Char.is_alpha c || c = '_'

  let read_ident lexer =
    let* c = Reader.peek lexer.reader in
    if is_ident_start c
    then (
      let raw =
        Reader.read_while (fun c -> Char.is_alphanumeric c || c = '_') lexer.reader
      in
      let ident : Token.Types.ident = { raw; name = raw } in
      Some (Token.Types.Ident ident))
    else None
  ;;

  let read_hex lexer =
    let* c = Reader.peek lexer.reader in
    let* () = if c = '0' then Some () else None in
    let* x = Reader.peek2 lexer.reader in
    let* () = if x = 'x' then Some () else None in
    let recording = Reader.start_rec lexer.reader in
    Reader.advance lexer.reader;
    Reader.advance lexer.reader;
    let _ : string = lexer.reader |> Reader.read_while Char.is_hex_digit in
    let raw = Reader.finish_rec recording in
    Some (Token.Types.Number { raw })
  ;;

  let read_number lexer =
    let* c = Reader.peek lexer.reader in
    let* () = if Char.is_digit c then Some () else None in
    let seen_dot = ref (Reader.prev lexer.reader = Some '.') in
    let raw =
      lexer.reader
      |> Reader.read_while (function
        | '.' when not !seen_dot ->
          seen_dot := true;
          (match Reader.peek2 lexer.reader with
           | Some c when Char.is_digit c -> true
           | _ -> false)
        | c when Char.is_digit c -> true
        | _ -> false)
    in
    Some (Token.Types.Number { raw })
  ;;

  let read_punct lexer =
    let reader = lexer.reader in
    let* c = Reader.peek reader in
    let is_punct : char -> bool = function
      | '_' | '\'' | '"' -> false
      | c when Char.is_whitespace c -> false
      | c when Char.is_alphanumeric c -> false
      | _ -> true
    in
    let is_single_punct = function
      | c when String.contains "@(){}[]&^$;\\," c -> true
      | _ -> false
    in
    if is_punct c
    then (
      Reader.advance reader;
      let raw =
        match is_single_punct c with
        | true -> String.make 1 c
        | false ->
          String.make 1 c
          ^ Reader.read_while (fun c -> is_punct c && not (is_single_punct c)) reader
      in
      let token : Token.Types.punct = { raw } in
      Some (Token.Types.Punct token))
    else None
  ;;

  let read_line_comment lexer =
    let* c = Reader.peek lexer.reader in
    let* () = if c = '#' then Some () else None in
    let raw = lexer.reader |> Reader.read_while (fun c -> c <> '\n') in
    Some (Token.Types.Comment { raw; ty = Line })
  ;;

  let read_block_comment lexer : Token.Shape.t option =
    let* () = if Reader.peek lexer.reader = Some '(' then Some () else None in
    let* () = if Reader.peek2 lexer.reader = Some '#' then Some () else None in
    let raw = Reader.start_rec lexer.reader in
    let start_pos = lexer.reader.position in
    Reader.advance lexer.reader;
    Reader.advance lexer.reader;
    let nestiness = ref 1 in
    while !nestiness > 0 do
      if Reader.peek lexer.reader = Some '#' && Reader.peek2 lexer.reader = Some ')'
      then (
        Reader.advance lexer.reader;
        Reader.advance lexer.reader;
        nestiness := !nestiness - 1)
      else if Reader.peek lexer.reader = Some '(' && Reader.peek2 lexer.reader = Some '#'
      then (
        Reader.advance lexer.reader;
        Reader.advance lexer.reader;
        nestiness := !nestiness + 1)
      else if Reader.peek lexer.reader = None
      then
        error
          "Unclosed block comment (start at %a) @{<dim>at %a@}"
          Position.print
          start_pos
          Position.print
          lexer.reader.position
      else Reader.advance lexer.reader
    done;
    Some (Token.Types.Comment { raw = Reader.finish_rec raw; ty = Block })
  ;;

  let read_raw_ident lexer : Token.Shape.t option =
    let* () = if Reader.peek lexer.reader = Some '@' then Some () else None in
    let* () = if Reader.peek2 lexer.reader = Some '"' then Some () else None in
    let raw = Reader.start_rec lexer.reader in
    Reader.advance lexer.reader;
    let string_token = read_string_impl lexer in
    match string_token.parts with
    | [ Content { raw = name; _ } ] ->
      Some (Token.Types.Ident { raw = Reader.finish_rec raw; name })
    | _ -> error "Can't use interpolated string in raw ident"
  ;;

  let read_raw_keyword lexer : Token.Shape.t option =
    let* () = if Reader.peek lexer.reader = Some '@' then Some () else None in
    let* peek2 = Reader.peek2 lexer.reader in
    let* () = if is_ident_start peek2 then Some () else None in
    let raw = Reader.start_rec lexer.reader in
    Reader.advance lexer.reader;
    let _ident = read_ident lexer |> Option.get in
    Some (Token.Types.Punct { raw = Reader.finish_rec raw })
  ;;

  let rules =
    [ read_whitespace
    ; read_eof
    ; read_raw_ident
    ; read_raw_keyword
    ; read_ident
    ; read_hex
    ; read_number
    ; read_string
    ; read_line_comment
    ; read_block_comment
    ; read_punct
    ]
  ;;
end

let default_rules : rule list = DefaultRules.rules

let read_all : rule list -> source -> Token.t list =
  fun rules source ->
  let lexer = init rules source in
  let found_eof = ref false in
  let dispenser : unit -> Token.t option =
    fun () ->
    if !found_eof
    then None
    else (
      let token = next lexer in
      (match token.shape with
       | Eof -> found_eof := true
       | _ -> ());
      Some token)
  in
  List.of_seq (Seq.of_dispenser dispenser)
;;

type recording =
  { recording : Recording.t
  ; lexer : lexer
  }

let start_rec : lexer -> recording =
  fun lexer ->
  let recording : Recording.t = { tokens_rev = [] } in
  RecordingTable.add lexer.recordings recording ();
  { recording; lexer }
;;

let stop_rec : recording -> Token.t list =
  fun { recording; lexer } ->
  RecordingTable.remove lexer.recordings recording;
  recording.tokens_rev |> List.rev
;;

let maybe_convert_to_raw_ident : string -> string =
  fun name ->
  let lexer =
    init
      DefaultRules.rules
      { contents = name; uri = Uri.fake "maybe_convert_to_raw_ident" }
  in
  let convert_to_raw =
    match DefaultRules.read_ident lexer with
    | None -> true
    | Some _ident -> lexer.reader |> Reader.peek |> Option.is_some
  in
  if convert_to_raw then make_string "@%a" String.print_debug name else name
;;
