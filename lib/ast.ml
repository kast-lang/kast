open Prelude
open Span
open Lexer
open Syntax

type 'a node =
  | Nothing of { data : 'a }
  | Simple of { token : token; data : 'a }
  | Complex of { def : syntax_def; values : 'a node StringMap.t; data : 'a }
  | Syntax of { def : syntax_def; value : 'a node; data : 'a }

let rec map : 'a 'b. ('a -> 'b) -> 'a node -> 'b node =
 fun f node ->
  match node with
  | Nothing { data } -> (Nothing { data = (f data : 'b) } : 'b node)
  | Simple { token; data } -> (Simple { token; data = (f data : 'b) } : 'b node)
  | Complex { def; values; data } ->
      (Complex
         { def; values = StringMap.map (map f) values; data = (f data : 'b) }
        : 'b node)
  | Syntax { def; value; data } ->
      (Syntax { def; value = map f value; data = (f data : 'b) } : 'b node)

let data : 'a. 'a node -> 'a = function
  | Nothing { data }
  | Simple { data; _ }
  | Complex { data; _ }
  | Syntax { data; _ } ->
      data

type value = span node

let show_impl : 'a. bool -> 'a node -> string =
 fun show_names value ->
  let rec impl = function
    | Nothing _ -> "nothing"
    | Simple { token; _ } -> Lexer.show token
    | Complex { def; values; _ } ->
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
    | Syntax { def; value; _ } ->
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

let show : 'a. 'a node -> string = fun node -> show_impl false node
let show_verbose : 'a. 'a node -> string = fun node -> show_impl true node

type 'a peekable = { head : 'a option; tail : 'a Seq.t }

let peek : 'a. 'a peekable ref -> 'a option =
 fun (q : 'a peekable ref) : 'a option ->
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
  (* Log.never
     ("peeked "
     ^
     match result with Some token -> Lexer.show_spanned token | None -> "<eof>"); *)
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

type parse_state = {
  syntax : syntax;
  start : pos option;
  pos : pos;
  until : priority;
  state : keyword_parse_state;
  values : span node list;
  prev_value : span node option;
  joining : bool;
}

let parse (syntax : syntax) (tokens : token spanned Seq.t) (filename : filename)
    : span node =
  let tokens : tokens = ref { head = None; tail = tokens } in
  let consume_token () =
    match pop tokens with
    | Some spanned_token ->
        Log.never ("consumed " ^ Lexer.show_spanned spanned_token)
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
  let rec parse_until (parse_state : parse_state) : span node =
    let should_continue_with (continuing : bool) (next : keyword_parse_state) :
        bool =
      Log.trace
        ("checking if need to continue "
        ^ Int.to_string parse_state.until.after
        ^ " with " ^ Bool.to_string continuing ^ ", "
        ^ show_priority next.priority);
      let result =
        if parse_state.until.after == Int.min_int then true
        else
          match Int.compare parse_state.until.after next.priority.before with
          | x when x < 0 -> true
          | 0 -> (
              if parse_state.until.assoc <> next.priority.assoc then
                failwith "same priority different associativity?";
              if continuing && not parse_state.state.root then true
              else
                match parse_state.until.assoc with
                | Left -> false
                | Right -> true)
          | _ -> false
      in
      Log.trace ("result " ^ Bool.to_string result);
      result
    in
    let finish () : span node =
      match
        BoolMap.find_opt
          (Option.is_some parse_state.prev_value)
          parse_state.state.finish
      with
      | Some finished -> (
          let values = maybe_join parse_state.values parse_state.prev_value in
          let values = List.rev values in

          Log.trace
            ("finishing " ^ parsed_name finished ^ " with "
            ^ Int.to_string (List.length values)
            ^ " values");
          match finished with
          | Nothing -> (
              match values with
              | [] -> Nothing { data = failwith "todo nothing" }
              | _ -> failwith "values on nothing?")
          | Simple -> (
              match values with
              | [ value ] -> value
              | _ -> failwith "Expected a single value wtf")
          | Complex def ->
              let rec collect_values parts values : span node StringMap.t =
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
              Complex
                {
                  def;
                  values = collect_values def.parts values;
                  data =
                    {
                      start = Option.get parse_state.start;
                      finish = parse_state.pos;
                      file = filename;
                    };
                })
      | None -> failwith "Can't finish"
    in
    match peek tokens with
    | None -> finish ()
    | Some { value = Ident "syntax"; span } ->
        if Option.is_some parse_state.prev_value then
          failwith "syntax after value";
        let def = parse_syntax () in
        let new_syntax = add_syntax def parse_state.syntax in
        let start_state =
          { (start_state new_syntax) with next = EdgeMap.empty }
        in
        Log.trace ("new syntax = " ^ Syntax.show new_syntax);
        Log.trace ("starting until " ^ show_priority start_state.priority);
        let value_after =
          parse_until
            {
              syntax = new_syntax;
              start = None;
              pos = parse_state.pos;
              until = start_state.priority;
              state = start_state;
              values = [];
              prev_value = None;
              joining = false;
            }
        in
        let value =
          Syntax
            {
              def;
              value = value_after;
              data = { span with finish = (data value_after).finish };
            }
        in
        Log.trace ("parsed " ^ show value);
        value
    | Some spanned_token -> (
        let token = spanned_token.value in
        let raw_token = Lexer.raw token in
        Log.trace ("peek = " ^ raw_token);
        let edge : Edge.t =
          {
            value_before_keyword = Option.is_some parse_state.prev_value;
            keyword = raw_token;
          }
        in
        let next_with state = EdgeMap.find_opt edge state.next in
        match next_with parse_state.state with
        | Some next -> (
            Log.trace ("considering continuing with " ^ show_edge edge);
            match should_continue_with true next with
            | true ->
                consume_token ();
                Log.trace ("continued " ^ show_edge edge);
                parse_until
                  {
                    syntax = parse_state.syntax;
                    start =
                      (match parse_state.start with
                      | Some start -> Some start
                      | None -> Some spanned_token.span.start);
                    pos = spanned_token.span.finish;
                    until = next.priority;
                    state = next;
                    values =
                      maybe_join parse_state.values parse_state.prev_value;
                    prev_value = None;
                    joining = false;
                  }
            | false ->
                Log.trace ("should not continue with " ^ show_edge edge);
                finish ())
        | None -> (
            match next_with (start_state parse_state.syntax) with
            | Some new_state -> (
                match should_continue_with false new_state with
                | true ->
                    consume_token ();
                    Log.trace ("started " ^ show_edge edge);
                    let value =
                      parse_until
                        {
                          syntax = parse_state.syntax;
                          start =
                            (match parse_state.start with
                            | Some start -> Some start
                            | None -> Some spanned_token.span.start);
                          pos = spanned_token.span.finish;
                          until = new_state.priority;
                          state = new_state;
                          values = Option.to_list parse_state.prev_value;
                          prev_value = None;
                          joining = false;
                        }
                    in
                    Log.trace ("parsed as " ^ show value);
                    parse_until
                      {
                        syntax = parse_state.syntax;
                        start = parse_state.start;
                        pos = (data value).finish;
                        until = parse_state.until;
                        state = parse_state.state;
                        values = parse_state.values;
                        prev_value = Some value;
                        joining = false;
                      }
                | false ->
                    Log.trace ("should not start with " ^ show_edge edge);
                    finish ())
            | None -> (
                match parse_state.prev_value with
                | Some prev_value -> (
                    match parse_state.syntax.join with
                    | Some join_state
                      when should_continue_with false join_state
                           && not parse_state.joining ->
                        Log.trace "joining";
                        let value =
                          parse_until
                            {
                              syntax = parse_state.syntax;
                              start = Some (data prev_value).start;
                              pos = parse_state.pos;
                              until = join_state.priority;
                              state = join_state;
                              values = [ prev_value ];
                              prev_value = None;
                              joining = false;
                            }
                        in
                        Log.trace ("parsed as " ^ show value);
                        let joined = value <> prev_value in
                        parse_until
                          {
                            syntax = parse_state.syntax;
                            start = parse_state.start;
                            pos = (data value).finish;
                            until = parse_state.until;
                            state = parse_state.state;
                            values = parse_state.values;
                            prev_value = Some value;
                            joining = not joined;
                          }
                    | _ -> finish ())
                | None -> (
                    match
                      StringSet.find_opt raw_token parse_state.syntax.keywords
                    with
                    | Some _ -> finish ()
                    | None -> (
                        match token with
                        | String _ | Ident _ | Number _ ->
                            consume_token ();
                            Log.trace ("simple " ^ Lexer.show token);
                            parse_until
                              {
                                syntax = parse_state.syntax;
                                start =
                                  (match parse_state.start with
                                  | Some start -> Some start
                                  | None -> Some spanned_token.span.start);
                                pos = spanned_token.span.finish;
                                until = parse_state.until;
                                state = parse_state.state;
                                values = parse_state.values;
                                prev_value =
                                  Some
                                    (Simple { token; data = spanned_token.span });
                                joining = false;
                              }
                        | Punctuation _ ->
                            failwith "punctuation in place of value")))))
  in
  let start_state = { (start_state syntax) with next = EdgeMap.empty } in
  try
    let result =
      parse_until
        {
          syntax;
          start = None;
          pos = Span.start_pos;
          until = start_state.priority;
          state = start_state;
          values = [];
          prev_value = None;
          joining = false;
        }
    in
    if Option.is_some (peek tokens) then failwith "expected eof";
    result
  with Failure f ->
    failwith
      ("at " ^ Span.filename filename ^ ":"
      ^ (match peek tokens with
        | Some token -> show_spanned token
        | None -> "eof")
      ^ ": " ^ f)
