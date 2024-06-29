type assoc = Left | Right
type def_part = Keyword of string | Binding of string

type syntax_def = {
  name : string;
  assoc : assoc;
  priority : int;
  parts : def_part list;
}

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module BoolMap = Map.Make (Bool)

type priority = { before : int; after : int; assoc : assoc }

let need_pop prev next =
  let x = Int.compare prev.after next.after in
  if x < 0 then false
  else if x > 0 then true
  else (
    if prev.assoc <> next.assoc then failwith "panic";
    match prev.assoc with Left -> true | Right -> false)

type parsed = Simple | Complex of syntax_def

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
  finish : parsed BoolMap.t ref;
  next : edges ref;
}

and edges = keyword_parse_state EdgeMap.t

type syntax = {
  keywords : StringSet.t ref;
  join : keyword_parse_state option ref;
  starts : edges ref;
}

let start_state (syntax : syntax) : keyword_parse_state =
  {
    priority = { before = 0; after = Int.min_int; assoc = Left };
    finish = ref (BoolMap.singleton true Simple);
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
  match parsed with Simple -> "simple" | Complex def -> def.name

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
      !(state.finish);
    do_edges !(state.next)
  in
  do_state (start_state syntax);
  !result

let ensure_edge_exists (edges : edges ref) (edge : Edge.t) (priority : priority)
    : keyword_parse_state =
  edges :=
    EdgeMap.update edge
      (function
        | Some prev -> Some prev
        | None ->
            Some
              { priority; finish = ref BoolMap.empty; next = ref EdgeMap.empty })
      !edges;
  let state = EdgeMap.find edge !edges in
  if state.priority <> priority then failwith "wrong priority";
  state

let add_syntax (def : syntax_def) (syntax : syntax) =
  match def.parts with
  | [ Binding _; Binding _ ] ->
      syntax.join :=
        Some
          {
            priority =
              { before = def.priority; after = def.priority; assoc = def.assoc };
            finish =
              ref (BoolMap.of_list [ (false, Simple); (true, Complex def) ]);
            next = ref EdgeMap.empty;
          }
  | _ ->
      let edges = ref syntax.starts in
      let finish = ref (ref BoolMap.empty) in
      let value_before = ref false in
      let keyword_before = ref false in
      let rec has_keyword_in list =
        match list with
        | Keyword _ :: _ -> true
        | _ :: tail -> has_keyword_in tail
        | [] -> false
      in
      let rec has_keyword_after index list =
        if index < 0 then has_keyword_in list
        else
          match list with
          | _ :: tail -> has_keyword_after (index - 1) tail
          | [] -> false
      in
      List.iteri
        (fun index part ->
          let keyword_after : bool = has_keyword_after index def.parts in
          match part with
          | Keyword keyword ->
              syntax.keywords := StringSet.add keyword !(syntax.keywords);
              let state =
                ensure_edge_exists !edges
                  { value_before_keyword = !value_before; keyword }
                  {
                    before =
                      (if !keyword_before then Int.min_int else def.priority);
                    after =
                      (if keyword_after then Int.min_int else def.priority);
                    assoc = def.assoc;
                  }
              in
              value_before := false;
              finish := state.finish;
              edges := state.next
          | Binding _name ->
              if !value_before then failwith "two bindings after eachother wtf";
              value_before := true)
        def.parts;
      !finish :=
        BoolMap.update !value_before
          (fun prev ->
            if Option.is_some prev then failwith "conflict";
            Some (Complex def))
          !(!finish)

let make_syntax (defs : syntax_def list) : syntax =
  let result =
    {
      keywords = ref StringSet.empty;
      join = ref None;
      starts = ref EdgeMap.empty;
    }
  in
  List.iter (fun def -> add_syntax def result) defs;
  result
