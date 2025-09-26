open Std

type ocaml_ast =
  | RawCode of string
  | UnitType
  | Bool of bool
  | Int32 of int32
  | String of string
  | Tuple of ocaml_ast list
  | Placeholder
  | Var of string
  | LetThen of let_then list
  | Field of {
      obj : ocaml_ast;
      field : string;
    }
  | Fun of {
      args : ocaml_ast list;
      body : ocaml_ast;
    }
  | Call of {
      f : ocaml_ast;
      arg : ocaml_ast;
    }
  | Scope of ocaml_ast
  | If of {
      cond : ocaml_ast;
      then_case : ocaml_ast;
      else_case : ocaml_ast;
    }
  | Match of {
      expr : ocaml_ast;
      branches : match_branch list;
    }

and let_then =
  | Let of {
      bindings : let_binding list;
      recursive : bool;
    }
  | Then of ocaml_ast

and match_branch = {
  pattern : ocaml_ast;
  body : ocaml_ast;
}

and let_binding = {
  pattern : ocaml_ast;
  value : ocaml_ast;
}

type t = ocaml_ast

let unit_value = Tuple []

let single_let binding =
  LetThen [ Let { recursive = false; bindings = [ binding ] } ]

let rec print (fmt : formatter) (ast : ocaml_ast) : unit =
  match ast with
  | RawCode s -> fprintf fmt "@{<blue>%s@}" s
  | UnitType -> fprintf fmt "@{<magenta>unit@}"
  | Bool value -> fprintf fmt "@{<magenta>%B@}" value
  | Int32 value -> fprintf fmt "@{<italic>%ld@}" value
  | String value -> fprintf fmt "@{<green>%S@}" value
  | Tuple values ->
      fprintf fmt "@{<magenta>(@}@,";
      values
      |> List.iteri (fun i value ->
             fprintf fmt (if i <> 0 then "@{<magenta>,@} " else "  ");
             fprintf fmt "@[<v>%a@]@," print value);
      fprintf fmt "@{<magenta>)@}"
  | Placeholder -> fprintf fmt "_"
  | Var name -> fprintf fmt "%s" name
  | Field { obj; field } -> fprintf fmt "%a@{<magenta>.@}%s" print obj field
  | Fun { args; body } ->
      fprintf fmt "@{<magenta>fun@}";
      args
      |> List.iter (fun arg ->
             fprintf fmt " ";
             print fmt arg);
      fprintf fmt " @{<magenta>->@}@;<0 2>@[<v>%a@]" print body
  (* | Then exprs ->
      let length = exprs |> List.length in
      exprs
      |> List.iteri (fun i expr ->
             print fmt expr;
             if i + 1 < length then fprintf fmt "@{<magenta>;@}@,") *)
  | Call { f; arg } ->
      fprintf fmt "%a @{<magenta>(@}%a@{<magenta>)@}" print f print arg
  | Scope expr ->
      fprintf fmt "@{<magenta>(@}@;<0 2>@[<v>%a@]@,@{<magenta>)@}" print expr
  | If { cond; then_case; else_case } ->
      fprintf fmt
        "@{<magenta>if@} %a @{<magenta>then@}@;<0 2>@[<v>%a@]@,@{<magenta>else@}@;<0 2>@[<v>%a@]"
        print cond print then_case print else_case
  | LetThen let_thens ->
      let let_then_len = let_thens |> List.length in
      let_thens
      |> List.iteri (fun i let_then ->
             match let_then with
             | Let { bindings; recursive } ->
                 bindings
                 |> List.iteri (fun i { pattern; value } ->
                        fprintf fmt "@{<magenta>%s@} %a @{<magenta>=@} %a@,"
                          (if i = 0 then if recursive then "let rec" else "let"
                           else "and")
                          print pattern print value);
                 fprintf fmt "@{<magenta>in@}@,";
                 if i + 1 >= let_then_len then fprintf fmt "@{<magenta>()@}"
             | Then expr ->
                 print fmt expr;
                 if i + 1 < let_then_len then fprintf fmt "@{<magenta>;@}@,")
  | Match { expr; branches } ->
      fprintf fmt "@{<magenta>(match@} %a @{<magenta>with@}" print expr;
      branches
      |> List.iter (fun { pattern; body } ->
             fprintf fmt "@,@{<magenta>|@} %a @{<magenta>->@} %a" print pattern
               print body);
      fprintf fmt "@{<magenta>)@}"

let merge_let_then a b =
  let a =
    match a with
    | LetThen a -> a
    | _ -> [ Then a ]
  in
  let b =
    match b with
    | LetThen b -> b
    | _ -> [ Then b ]
  in
  LetThen (a @ b)
