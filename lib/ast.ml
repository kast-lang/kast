open Prelude
open Span
open Lexer
open Syntax

let show_spanned (token : token spanned) : string =
  let span = token.span in
  "at " ^ Span.show span ^ " (" ^ Lexer.show token ^ ")"

type value =
  | Nothing
  | Simple of token spanned
  | Complex of { def : syntax_def; values : value StringMap.t }
  | Syntax of { def : syntax_def; value : value }

let show_impl (show_names : bool) (value : value) : string =
  let rec impl = function
    | Nothing -> "nothing"
    | Simple token -> Lexer.show token
    | Complex { def; values } ->
        "(" ^ def.name
        ^ List.fold_left
            (fun prev part ->
              match part with
              | Keyword _ -> prev
              | Binding name ->
                  let value = StringMap.find name values in
                  (if prev = "" then " " else prev ^ " ")
                  ^ (if show_names then name ^ "=" else "")
                  ^ impl value)
            "" def.parts
        ^ ")"
    | Syntax { def; value } ->
        "(syntax " ^ def.name ^ " "
        ^ (match def.assoc with Left -> "<-" | Right -> "->")
        ^ " " ^ Int.to_string def.priority ^ " ="
        ^ List.fold_left
            (fun s part ->
              s ^ " "
              ^
              match part with
              | Binding name -> name
              | Keyword keyword -> "\"" ^ keyword ^ "\"")
            "" def.parts
        ^ "; " ^ impl value ^ ")"
  in
  impl value

let show = show_impl false
let show_verbose = show_impl true

type 'a peekable = { head : 'a option; tail : 'a Seq.t }

let peek (q : 'a peekable ref) : 'a option =
  let result =
    match !q.head with
    | Some value -> Some value
    | None ->
        (q :=
           match !q.tail () with
           | Cons (head, tail) -> { head = Some head; tail }
           | Nil -> { head = None; tail = Seq.empty });
        !q.head
  in
  Log.never
    ("peeked "
    ^ match result with Some token -> Lexer.show token | None -> "<eof>");
  result

let pop (q : 'a peekable ref) : 'a option =
  let result = peek q in
  q := { !q with head = None };
  result

type tokens = token spanned peekable ref

let maybe_join list option =
  match option with Some value -> value :: list | None -> list

let show_edge (edge : Edge.t) =
  match edge.value_before_keyword with
  | true -> "_ " ^ edge.keyword
  | false -> edge.keyword

let parse (syntax : syntax) (tokens : token spanned Seq.t) : value =
  let tokens : tokens = ref { head = None; tail = tokens } in
  let consume_token () =
    match pop tokens with
    | Some token -> Log.never ("consumed " ^ Lexer.show token)
    | None -> failwith "expected to have some token"
  in
  let parse_syntax () : syntax_def =
    match peek tokens with
    | Some { value = Ident "syntax"; _ } ->
        consume_token ();
        let name =
          match pop tokens with
          | Some { value = Ident name; _ } -> name
          | _ -> failwith "expected ident name"
        in
        let assoc =
          match pop tokens with
          | Some { value = Punctuation "<-"; _ } -> Left
          | Some { value = Punctuation "->"; _ } -> Right
          | _ -> failwith "expected associativity (<- or ->)"
        in
        let priority =
          match pop tokens with
          | Some { value = Number s; _ } -> (
              match int_of_string_opt s with
              | Some priority -> priority
              | None -> failwith "failed to parse priority")
          | _ -> failwith "expected priority (number)"
        in
        (match pop tokens with
        | Some { value = Punctuation "="; _ } -> ()
        | _ -> failwith "expected =");
        let rec collect_parts () =
          match peek tokens with
          | Some { value = Punctuation ";"; _ } ->
              consume_token ();
              []
          | None -> []
          | Some { value = token; _ } ->
              consume_token ();
              let part =
                match token with
                | String { value = keyword; _ } -> Keyword keyword
                | Ident name -> Binding name
                | _ -> failwith "expected stringified keyword or binding name"
              in
              part :: collect_parts ()
        in
        { name; assoc; priority; parts = collect_parts () }
    | _ -> failwith "expected syntax"
  in
  let rec parse_until (syntax : syntax) (until : priority)
      (state : keyword_parse_state) (values : value list)
      (prev_value : value option) (joining : bool) : value =
    let should_continue_with (next : keyword_parse_state) : bool =
      if until.after == Int.min_int then true
      else
        match Int.compare until.after next.priority.before with
        | x when x < 0 -> true
        | 0 -> (
            if until.assoc <> next.priority.assoc then
              failwith "same priority different associativity?";
            match until.assoc with Left -> false | Right -> true)
        | _ -> false
    in
    let finish () : value =
      match BoolMap.find_opt (Option.is_some prev_value) state.finish with
      | Some finished -> (
          let values = maybe_join values prev_value in
          let values = List.rev values in

          Log.trace
            ("finishing " ^ parsed_name finished ^ " with "
            ^ Int.to_string (List.length values)
            ^ " values");
          match finished with
          | Nothing -> (
              match values with
              | [] -> Nothing
              | _ -> failwith "values on nothing?")
          | Simple -> (
              match values with
              | [ value ] -> value
              | _ -> failwith "Expected a single value wtf")
          | Complex def ->
              let rec collect_values parts values : value StringMap.t =
                match (parts, values) with
                | [], [] -> StringMap.empty
                | [], _ -> failwith "too many values"
                | Keyword _ :: parts_tail, values ->
                    collect_values parts_tail values
                | Binding name :: parts_tail, value :: values_tail ->
                    Log.trace ("collected " ^ name ^ " = " ^ show value);
                    StringMap.union
                      (fun _key _a _b -> failwith "duplicate key")
                      (StringMap.singleton name value)
                      (collect_values parts_tail values_tail)
                | Binding name :: _parts_tail, [] ->
                    failwith ("not enough values (missing " ^ name ^ ")")
              in
              Complex { def; values = collect_values def.parts values })
      | None -> failwith "Can't finish"
    in
    match peek tokens with
    | None -> finish ()
    | Some { value = Ident "syntax"; _ } ->
        if Option.is_some prev_value then failwith "syntax after value";
        let def = parse_syntax () in
        let new_syntax = add_syntax def syntax in
        let start_state = start_state new_syntax in
        let value =
          Syntax
            {
              def;
              value =
                parse_until new_syntax start_state.priority start_state [] None
                  false;
            }
        in
        parse_until syntax until state values (Some value) false
    | Some spanned_token -> (
        let token = spanned_token.value in
        let raw_token = Lexer.raw token in
        let edge : Edge.t =
          {
            value_before_keyword = Option.is_some prev_value;
            keyword = raw_token;
          }
        in
        let next_with state = EdgeMap.find_opt edge state.next in
        match next_with state with
        | Some next -> (
            let should_continue =
              match should_continue_with next with
              | true -> true
              | false -> not state.root
            in
            match should_continue with
            | true ->
                consume_token ();
                Log.trace ("continued " ^ show_edge edge);
                let value =
                  parse_until syntax next.priority next
                    (maybe_join values prev_value)
                    None false
                in
                parse_until syntax until (start_state syntax) [] (Some value)
                  false
            | false ->
                Log.trace ("should not continue with " ^ show_edge edge);
                finish ())
        | None -> (
            match next_with (start_state syntax) with
            | Some new_state -> (
                match should_continue_with new_state with
                | true ->
                    consume_token ();
                    Log.trace ("started " ^ show_edge edge);
                    let value =
                      parse_until syntax new_state.priority new_state
                        (Option.to_list prev_value)
                        None false
                    in
                    Log.trace ("parsed as " ^ show value);
                    parse_until syntax until state values (Some value) false
                | false -> finish ())
            | None -> (
                match prev_value with
                | Some prev_value -> (
                    match syntax.join with
                    | Some join_state
                      when should_continue_with join_state && not joining ->
                        Log.trace "joining";
                        let value =
                          parse_until syntax join_state.priority join_state
                            [ prev_value ] None false
                        in
                        Log.trace ("parsed as " ^ show value);
                        let joined = value <> prev_value in
                        parse_until syntax until state values (Some value)
                          (not joined)
                    | _ -> finish ())
                | None -> (
                    match StringSet.find_opt raw_token syntax.keywords with
                    | Some _ -> finish ()
                    | None -> (
                        match token with
                        | String _ | Ident _ | Number _ ->
                            consume_token ();
                            Log.trace ("simple " ^ Lexer.show spanned_token);
                            parse_until syntax until state values
                              (Some (Simple spanned_token)) false
                        | Punctuation _ ->
                            failwith "punctuation in place of value")))))
  in
  let start_state = start_state syntax in
  try parse_until syntax start_state.priority start_state [] None false
  with Failure f ->
    failwith
      ("at "
      ^ (match peek tokens with
        | Some token -> show_spanned token
        | None -> "eof")
      ^ ": " ^ f)
