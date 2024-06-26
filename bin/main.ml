let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    let prefix_len = String.length prefix in
    Some (String.sub s prefix_len (String.length s - prefix_len))
  else None

let strip_suffix ~suffix s =
  if String.ends_with ~suffix s then
    let suffix_len = String.length suffix in
    Some (String.sub s (String.length s - suffix_len) suffix_len)
  else None

(* Copypasted from std *)
let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

(* most readable ocaml code *)
type split_whitespace_state = { seq : string Seq.t; start : int Option.t }

(* this function will appear in my dreams tonight *)
let split_whitespace : string -> string Seq.t =
 fun s ->
  let indices = Seq.take (String.length s) (Seq.ints 0) in
  let finish (state : split_whitespace_state) i =
    match state.start with
    | None -> state.seq
    | Some start ->
        Seq.append state.seq (Seq.return (String.sub s start (i - start)))
  in
  let f (cur : split_whitespace_state) i =
    let c = String.get s i in
    match is_space c with
    | true -> { seq = finish cur i; start = None }
    | false -> (
        match cur.start with
        | Some _ -> cur
        | None -> { seq = cur.seq; start = Some i })
  in
  finish
    (Seq.fold_left f { seq = Seq.empty; start = None } indices)
    (String.length s)

module Lexer = struct
  type token = Ident of string | OpenBracket | CloseBracket
  type state = { seq : token Seq.t; start : int Option.t }

  let show = function Ident s -> s | OpenBracket -> "(" | CloseBracket -> ")"

  let single_token = function
    | "(" -> OpenBracket
    | ")" -> CloseBracket
    | s -> Ident s

  let parse : string -> token Seq.t =
   fun s ->
    let indices = Seq.take (String.length s) (Seq.ints 0) in
    let finish (state : state) i =
      match state.start with
      | None -> state.seq
      | Some start ->
          Seq.append state.seq
            (Seq.return (single_token (String.sub s start (i - start))))
    in
    let f (cur : state) i =
      let c = String.get s i in
      let finish_before = is_space c || c = '(' || c = ')' in
      let finish_after = c = '(' || c = ')' in
      let new_start = match is_space c with true -> None | false -> Some i in
      let after =
        match finish_before with
        | true -> { seq = finish cur i; start = new_start }
        | false -> (
            match cur.start with
            | Some _ -> cur
            | None -> { seq = cur.seq; start = new_start })
      in
      match finish_after with
      | true -> { seq = finish after (i + 1); start = None }
      | false -> after
    in
    finish
      (Seq.fold_left f { seq = Seq.empty; start = None } indices)
      (String.length s)
end

module Ast = struct
  type t = Unit of string | List of t list

  exception UnclosedBracket

  let rec show : t -> string = function
    | Unit value -> value
    | List list -> "(" ^ String.concat " " (List.map show list) ^ ")"

  type state = { stack : t list }

  let parse : Lexer.token Seq.t -> t =
   fun seq ->
    let final_state =
      Seq.fold_left
        (fun state token ->
          match token with
          | Lexer.Ident s -> (
              match state.stack with
              | List head :: tail ->
                  { stack = List (List.append head [ Unit s ]) :: tail }
              | _ -> raise (Failure "whoops"))
          | Lexer.OpenBracket -> { stack = List [] :: state.stack }
          | Lexer.CloseBracket -> (
              match state.stack with
              | head :: List prev :: tail ->
                  { stack = List (List.append prev [ head ]) :: tail }
              | _ -> raise (Failure "haha")))
        { stack = [ List [] ] } seq
    in
    match final_state.stack with [ root ] -> root | _ -> raise UnclosedBracket
end

module Interpreter = struct
  type value = Int of int

  let show = function Int value -> string_of_int value
  let add a b = match (a, b) with Int a, Int b -> Int (a + b)

  let rec eval : Ast.t -> value = function
    | Ast.Unit s -> Int (int_of_string s)
    | Ast.List list -> (
        match list with
        | [ single ] -> eval single
        | [ Ast.Unit "+"; a; b ] -> add (eval a) (eval b)
        | _ -> raise (Failure "wut"))
end
;;

let rec main_loop () =
  print_string "> ";
  let line = read_line () in
  let tokens = Lexer.parse line in
  (* print_endline (String.concat " " (List.of_seq (Seq.map Lexer.show tokens))); *)
  let ast = Ast.parse tokens in
  (* print_endline (Ast.show ast); *)
  let value = Interpreter.eval ast in
  print_endline (Interpreter.show value);
  main_loop ()
in
main_loop ()
