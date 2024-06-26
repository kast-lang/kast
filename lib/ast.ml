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
