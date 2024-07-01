open Prelude

type assoc = Left | Right
type def_part = Keyword of string | Binding of string

type syntax_def = {
  name : string;
  assoc : assoc;
  priority : int;
  parts : def_part list;
}

type priority = { before : int; after : int; assoc : assoc }

let merge_priority a b =
  if a <> b then failwith "different priorities";
  a

let need_pop prev next =
  let x = Int.compare prev.after next.after in
  if x < 0 then false
  else if x > 0 then true
  else (
    if prev.assoc <> next.assoc then failwith "panic";
    match prev.assoc with Left -> true | Right -> false)

type parsed = Nothing | Simple | Complex of syntax_def

module Edge = struct
  type t = { value_before_keyword : bool; keyword : string }

  let compare { value_before_keyword; keyword } other =
    let x = Bool.compare value_before_keyword other.value_before_keyword in
    if x <> 0 then x
    else
      let x = String.compare keyword other.keyword in
      if x <> 0 then x else 0
end

module EdgeMap = Map.Make (Edge)

type keyword_parse_state = {
  priority : priority;
  finish : parsed BoolMap.t;
  next : edges;
}

and edges = keyword_parse_state EdgeMap.t

type syntax = {
  keywords : StringSet.t;
  join : keyword_parse_state option;
  starts : edges;
}

let empty : syntax =
  { keywords = StringSet.empty; join = None; starts = EdgeMap.empty }

let start_state (syntax : syntax) : keyword_parse_state =
  {
    priority = { before = 0; after = Int.min_int; assoc = Left };
    finish = BoolMap.of_list [ (false, Nothing); (true, Simple) ];
    next = syntax.starts;
  }

let show_assoc = function Left -> "left" | Right -> "right"

let show_priority (priority : priority) : string =
  "{ before = "
  ^ Int.to_string priority.before
  ^ "; after = "
  ^ Int.to_string priority.after
  ^ "; assoc = " ^ show_assoc priority.assoc ^ " }"

let parsed_name (parsed : parsed) : string =
  match parsed with
  | Nothing -> "nothing"
  | Simple -> "simple"
  | Complex def -> def.name

let show (syntax : syntax) : string =
  let prefix = ref "" in
  let result = ref "" in
  let rec do_edges (edges : edges) : unit =
    EdgeMap.iter
      (fun edge state ->
        result := !result ^ !prefix;
        if edge.value_before_keyword then result := !result ^ "_ ";
        result := !result ^ edge.keyword ^ " (\n";
        let old_prefix = !prefix in
        prefix := old_prefix ^ "  ";
        do_state state;
        prefix := old_prefix;
        result := !result ^ ")\n")
      edges
  and do_state (state : keyword_parse_state) : unit =
    result := !result ^ !prefix ^ "prio: " ^ show_priority state.priority ^ "\n";
    BoolMap.iter
      (fun final_value parsed ->
        result := !result ^ !prefix ^ "fin";
        if final_value then result := !result ^ " _";
        result := !result ^ " = " ^ parsed_name parsed ^ "\n")
      state.finish;
    do_edges state.next
  in
  do_state (start_state syntax);
  !result

let is_open_bracket s =
  String.length s = 1 && String.contains "([{" (String.get s 0)

let is_closing_bracket s =
  String.length s = 1 && String.contains ")]}" (String.get s 0)

let add_syntax (def : syntax_def) (syntax : syntax) : syntax =
  match def.parts with
  | [ Binding _; Binding _ ] ->
      {
        syntax with
        join =
          Some
            {
              priority =
                {
                  before = def.priority;
                  after = def.priority;
                  assoc = def.assoc;
                };
              finish = BoolMap.of_list [ (false, Simple); (true, Complex def) ];
              next = EdgeMap.empty;
            };
      }
  | _ ->
      let rec has_keyword = function
        | Keyword _ :: _tail -> true
        | Binding _ :: tail -> has_keyword tail
        | [] -> false
      in
      let rec add_to_state (state : keyword_parse_state) (parts : def_part list)
          (prev_was_value : bool) (had_keyword_before : bool) :
          keyword_parse_state =
        match parts with
        | [] ->
            {
              state with
              finish =
                BoolMap.update prev_was_value
                  (fun prev ->
                    if Option.is_some prev then failwith "conflict";
                    Some (Complex def))
                  state.finish;
            }
        | Binding _ :: remaining_parts ->
            if prev_was_value then failwith "two bindings after each other";
            add_to_state state remaining_parts true had_keyword_before
        | _ ->
            {
              state with
              next =
                add_to_edges state.next parts prev_was_value had_keyword_before;
            }
      and add_to_edges (edges : edges) (parts : def_part list)
          (prev_was_value : bool) (had_keyword_before : bool) : edges =
        match parts with
        | [] -> failwith "empty"
        | Binding _ :: remaining_parts ->
            if prev_was_value then failwith "two bindings after each other";
            add_to_edges edges remaining_parts true had_keyword_before
        | Keyword keyword :: remaining_parts ->
            EdgeMap.update
              { value_before_keyword = prev_was_value; keyword }
              (fun prev ->
                Some
                  (add_to_state
                     (let new_priority =
                        {
                          before =
                            (if is_closing_bracket keyword then Int.min_int
                             else def.priority);
                          after =
                            (if is_open_bracket keyword then Int.min_int
                             else def.priority);
                          assoc = def.assoc;
                        }
                      in
                      match prev with
                      | Some prev ->
                          {
                            prev with
                            priority = merge_priority prev.priority new_priority;
                          }
                      | None ->
                          {
                            priority = new_priority;
                            finish = BoolMap.empty;
                            next = EdgeMap.empty;
                          })
                     remaining_parts false true))
              edges
      in
      let collect_keywords =
        List.filter_map (function
          | Keyword keyword -> Some keyword
          | Binding _ -> None)
      in
      {
        syntax with
        starts = add_to_edges syntax.starts def.parts false false;
        keywords =
          StringSet.union syntax.keywords
            (StringSet.of_list (collect_keywords def.parts));
      }

let make_syntax (defs : syntax_def list) : syntax =
  List.fold_left
    (fun syntax def -> add_syntax def syntax)
    { keywords = StringSet.empty; join = None; starts = EdgeMap.empty }
    defs
