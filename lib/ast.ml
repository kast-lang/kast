open Prelude
open Span

type node =
  | Leaf of string
  | Node of { name : string; children : node spanned Map.Make(String).t }

type assoc = Left | Right
type def_part = Keyword of string | Binding of string

type syntax_def = {
  name : string;
  assoc : assoc;
  priority : int;
  def : def_part list;
}

(* call: left 50 = f args *)
(* add: left 20 = a "+" b *)
(* negate: left 100 = "-" x *)
(* if/then/else: left 10 = "if" cond "then" then "else" else *)

let syntax : syntax_def list =
  [
    {
      name = "call";
      assoc = Left;
      priority = 50;
      def = [ Binding "f"; Binding "args" ];
    };
    {
      name = "binary_add";
      assoc = Left;
      priority = 20;
      def = [ Binding "a"; Keyword "+"; Binding "b" ];
    };
  ]

module StateEdge = struct
  type t = { keyword : string option; value_before_keyword : bool }

  let compare a b =
    match Option.compare String.compare a.keyword b.keyword with
    | 0 -> Bool.compare a.value_before_keyword b.value_before_keyword
    | other -> other
end

type state_child = Keyword of string | Value of node

module EdgeMap = Map.Make (StateEdge)

type edges = state EdgeMap.t

and state = {
  finish_without_value : syntax_def option;
  finish_with_value : syntax_def option;
  continue : edges;
}

(* When I am done with this, there will be only two people in the whole world able to maintain the code. And I am not one of them. *)
module StringSet = Set.Make (String)

type syntax = { keywords : StringSet.t; root : state }

let part_keyword (part : def_part) : string option =
  match part with Keyword kw -> Some kw | Binding _ -> None

let rec append_states (state : state) (def : syntax_def)
    (prev_binding_name : string option) (remaining_parts : def_part list) :
    state =
  match remaining_parts with
  | [] -> (
      match prev_binding_name with
      | Some _ -> (
          match state.finish_with_value with
          | Some finish ->
              raise (Failure (finish.name ^ " conflicts with " ^ def.name))
          | None -> { state with finish_with_value = Some def })
      | None -> (
          match state.finish_without_value with
          | Some finish ->
              raise (Failure (finish.name ^ " conflicts with " ^ def.name))
          | None -> { state with finish_without_value = Some def }))
  | first :: remaining_parts -> (
      let update_with_edge edge =
        {
          state with
          continue =
            EdgeMap.update edge
              (fun prev ->
                let or_default =
                  match prev with
                  | Some prev -> prev
                  | None ->
                      {
                        finish_without_value = None;
                        finish_with_value = None;
                        continue = EdgeMap.empty;
                      }
                in
                let this_binding_name =
                  match first with
                  | Keyword _ -> None
                  | Binding name -> Some name
                in
                Some
                  (append_states or_default def this_binding_name
                     remaining_parts))
              state.continue;
        }
      in
      match first with
      | Keyword keyword ->
          update_with_edge
            {
              keyword = Some keyword;
              value_before_keyword = Option.is_some prev_binding_name;
            }
      | Binding this_binding_name -> (
          match prev_binding_name with
          | Some _ ->
              update_with_edge { keyword = None; value_before_keyword = true }
          | None ->
              append_states state def (Some this_binding_name) remaining_parts))

let construct_syntax : syntax_def list -> syntax =
 fun syntax_list ->
  let result =
    ref
      {
        keywords = StringSet.empty;
        root =
          {
            finish_with_value = None;
            finish_without_value = None;
            continue = EdgeMap.empty;
          };
      }
  in
  List.iter
    (fun def ->
      result :=
        {
          keywords =
            StringSet.union !result.keywords
              (StringSet.of_list (List.filter_map part_keyword def.def));
          root = append_states !result.root def None def.def;
        })
    syntax_list;
  !result

type kov = Keyword of string | Value of node

let kov_edge = function Keyword kw -> Some kw | Value _ -> None

type parse_state = { stack : state list; prev_value : node option }

(* #Clown *)
let parse (syntax : syntax) (tokens : Lexer.token spanned Seq.t) : node spanned
    =
  let parse_state : parse_state ref = ref { stack = []; prev_value = None } in
  let rec push_edge (edge : StateEdge.t) =
    let continue =
      List.find_mapi
        (fun index (state : state) ->
          match EdgeMap.find_opt edge state.continue with
          | Some new_state -> Some (index, new_state)
          | None -> None)
        !
    in
    match continue with
    | Some (index, new_state) ->
        for _ = 0 to index do
          pop_apply ()
        done;
        stack := new_state :: !stack
    | None -> ()
  and push (kov : kov) =
    match kov with Keyword keyword -> () | Value value -> ()
  and pop_apply () =
    (* look at this comment *)
    match !stack with
    | head :: tail ->
        (match head.finish with
        | Some name -> ()
        | None -> raise (Failure "expression is not finished"));
        stack := tail
    | [] -> raise (Failure "stack is empty")
  in
  Seq.iter
    (fun token ->
      let token_value = Lexer.token_value token.value in
      let kov =
        match StringSet.find_opt token_value syntax.keywords with
        | Some kw -> Keyword kw
        | None -> Value (Leaf token_value)
      in
      push kov)
    tokens;
  raise (Failure "todo")
