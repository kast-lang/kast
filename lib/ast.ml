open Span
open Lexer
open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type value =
  | Simple of token spanned
  | Complex of { def : syntax_def; values : value StringMap.t }

let rec show : value -> string = function
  | Simple token -> Lexer.show token
  | Complex { def; values } ->
      "(" ^ def.name
      ^ List.fold_left
          (fun prev part ->
            match part with
            | Keyword _ -> prev
            | Binding name ->
                let value = StringMap.find name values in
                (if prev = "" then " " else prev ^ ", ")
                ^ name ^ "=" ^ show value)
          "" def.parts
      ^ ")"

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
  let rec parse_until (until : priority) (state : keyword_parse_state)
      (values : value list) (prev_value : value option) (joining : bool) : value
      =
    let should_continue_with (next : keyword_parse_state) : bool =
      match Int.compare until.after next.priority.before with
      | x when x < 0 -> true
      | 0 -> (
          if until.assoc <> next.priority.assoc then
            failwith "same priority different associativity?";
          match until.assoc with Left -> false | Right -> true)
      | _ -> false
    in
    let consume_token () =
      match pop tokens with
      | Some token -> Log.never ("consumed " ^ Lexer.show token)
      | None -> failwith "expected to have some token"
    in
    let finish () : value =
      match BoolMap.find_opt (Option.is_some prev_value) !(state.finish) with
      | Some finished -> (
          let values = maybe_join values prev_value in
          let values = List.rev values in

          Log.trace
            ("finishing " ^ parsed_name finished ^ " with "
            ^ Int.to_string (List.length values)
            ^ " values");
          match finished with
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
    | Some token -> (
        let (Ident s) = token.value in
        let edge : Edge.t =
          { value_before_keyword = Option.is_some prev_value; keyword = s }
        in
        let next_with state = EdgeMap.find_opt edge !(state.next) in
        match next_with state with
        | Some next -> (
            match should_continue_with next with
            | true ->
                consume_token ();
                Log.trace ("continued " ^ show_edge edge);
                let value =
                  parse_until next.priority next
                    (maybe_join values prev_value)
                    None false
                in
                parse_until until (start_state syntax) [] (Some value) false
            | false -> finish ())
        | None -> (
            match next_with (start_state syntax) with
            | Some new_state -> (
                match should_continue_with new_state with
                | true ->
                    consume_token ();
                    Log.trace ("started " ^ show_edge edge);
                    let value =
                      parse_until new_state.priority new_state
                        (Option.to_list prev_value)
                        None false
                    in
                    Log.trace ("parsed as " ^ show value);
                    parse_until until state values (Some value) false
                | false -> finish ())
            | None -> (
                match prev_value with
                | Some prev_value -> (
                    match !(syntax.join) with
                    | Some join_state
                      when should_continue_with join_state && not joining ->
                        Log.trace "joining";
                        let value =
                          parse_until join_state.priority join_state
                            [ prev_value ] None false
                        in
                        Log.trace ("parsed as " ^ show value);
                        let joined = value <> prev_value in
                        parse_until until state values (Some value) (not joined)
                    | _ -> finish ())
                | None -> (
                    match StringSet.find_opt s !(syntax.keywords) with
                    | Some _ -> finish ()
                    | None ->
                        consume_token ();
                        Log.trace ("simple " ^ Lexer.show token);
                        parse_until until state values (Some (Simple token))
                          false))))
  in

  let start_state = start_state syntax in
  parse_until start_state.priority start_state [] None false
