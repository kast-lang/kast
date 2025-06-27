open Format

(* type expr =
  | Ident of string
  | String of string
  | Then of expr list
  | Stmt of expr
  | Use of expr
  | If of {
      cond : expr;
      then_case : expr;
      else_case : expr;
    }
  | Or of expr list
  | And of expr list
  | Pipe of {
      arg : expr;
      f : expr;
    }
  | Apply of {
      f : expr;
      arg : expr;
    }
  | Scope of expr

let rec format : expr -> unit = function
  | Ident s -> printf "%s" s
  | String s -> printf "%S" s
  | Then list ->
      let first = ref true in
      list
      |> List.iter (fun e ->
             if !first then first := false else printf ";";
             format e)
  | Stmt e ->
      format e;
      printf ";"
  | Or list -> 

(* 
if a then b else c;
if ( a or b ) and ( c or d )
then (
  print "true" ;
  print "true" ;
  print "true" ;
  print "true" ;
) else (
  print "false"; *)
);
 *)
(* let example : expr =
  Then
    [
      If { cond = Ident "a"; then_case = Ident "b"; else_case = Ident "c" };
      If
        {
          cond =
            And [ Or [ Ident "a"; Ident "b" ]; Or [ Ident "c"; Ident "d" ] ];
          then_case =
            Scope
              (Then
                 [
                   Apply { f = Ident "print"; arg = String "hello" };
                   Apply { f = Ident "print"; arg = String "hello" };
                   Apply { f = Ident "print"; arg = String "hello" };
                   Apply { f = Ident "print"; arg = String "hello" };
                   Stmt (Apply { f = Ident "print"; arg = String "hello" });
                 ]);
          else_case =
            Scope (Stmt (Apply { f = Ident "print"; arg = String "hello" }));
        };
    ]

let () = format example *)
