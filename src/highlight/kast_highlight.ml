open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Parser = Kast_parser
module Ast = Kast_ast

type printer = {
  fmt : formatter;
  mutable position : position;
}

type output =
  | Term
  | Html

module type Output = sig
  val initialize : printer -> unit
  val finalize : printer -> unit
  val print_char : printer -> char -> unit
  val move_to : printer -> position -> unit
  val print_comment : printer -> Token.comment -> unit
  val print_keyword : printer -> Token.Shape.t -> unit
  val print_syntax_part : printer -> Token.Shape.t -> unit
  val print_value : printer -> Token.Shape.t -> unit
end

(* Highlight for a terminal *)
module Term : Output = struct
  let initialize = fun _ -> ()
  let finalize = fun _ -> ()

  let print_char printer c =
    printer.position <- Position.advance c printer.position;
    fprintf printer.fmt "%c" c

  (* TODO actual whitespace tokens? *)
  let move_to (printer : printer) (position : position) : unit =
    while printer.position.line < position.line do
      print_char printer '\n'
    done;
    while printer.position.column < position.column do
      print_char printer ' '
    done

  let print_comment (printer : printer) (comment : Token.comment) : unit =
    move_to printer comment.span.start;
    fprintf printer.fmt "@{<gray>%s@}" comment.shape.raw;
    printer.position <- comment.span.finish

  let print_keyword printer (token : Token.Shape.t) =
    match Token.Shape.raw token with
    | Some raw -> fprintf printer.fmt "@{<magenta>%s@}" raw
    | None -> ()

  let print_syntax_part printer (token : Token.Shape.t) =
    match Token.Shape.raw token with
    | Some raw -> fprintf printer.fmt "@{<yellow>%s@}" raw
    | None -> ()

  let print_value printer (token : Token.Shape.t) =
    let fmt = printer.fmt in
    match token with
    | Ident { raw; _ } -> fprintf fmt "%s" raw
    | String { raw; _ } -> fprintf fmt "@{<green>%s@}" raw
    | Number { raw; _ } -> fprintf fmt "@{<italic>%s@}" raw
    | Punct { raw; _ } -> fprintf fmt "%s" raw
    | Comment _ -> unreachable "<comment> value??"
    | Eof -> unreachable "<eof> value??"
end

(* Highlight as HTML *)
module Html : Output = struct
  let initialize { fmt; _ } =
    [
      "<style>";
      (*helix onedark theme*)
      "    /*fg colors*/";
      "    code .fg-none    { }";
      "    code .fg-white   { color: #ABB2BF; }";
      "    code .fg-grey    { color: #5C6370; }";
      "    code .fg-green   { color: #98C379; }";
      "    code .fg-yellow  { color: #E5C07B; }";
      "    code .fg-purple  { color: #C678DD; }";
      "    code .fg-magenta { color: #C678DD; }";
      "";
      (*currently unused: *)
      "    code .fg-blue { color: #61AFEF; }";
      "    code .fg-red { color: #E06C75; }";
      "    code .fg-gold { color: #D19A66; }";
      "    code .fg-cyan { color: #56B6C2; }";
      "    code .fg-black { color: #282C34; }";
      "    code .fg-light-black { color: #2C323C; }";
      "    code .fg-gray { color: #3E4452; }";
      "    code .fg-faint-gray { color: #3B4048; }";
      "    code .fg-light-gray { color: #5C6370; }";
      "    code .fg-linenr { color: #4B5263; }";
      "";
      "    /*bg colors*/";
      "    code .bg-black   { background-color: #282C34; }";
      "    pre.bg-black     { background-color: #282C34; }";
      "";
      "    /*decoration*/";
      "    code .font-italic   { font-style: italic; }";
      "</style>\n";
    ]
    |> List.map (fprintln fmt)
    |> ignore;
    fprintf fmt "<pre class=\"bg-black\"><code>"

  let finalize { fmt; _ } = fprintln fmt "</code></pre>"

  let print_str printer s color =
    fprintf printer "<span class=\"fg-%s\">%s</span>" color s

  let print_str_italic printer s color =
    fprintf printer "<span class=\"fg-%s font-italic\">%s</span>" color s

  let print_char printer c =
    printer.position <- Position.advance c printer.position;
    fprintf printer.fmt "<span class=\"fg-none\">%c</span>" c

  (* TODO actual whitespace tokens? *)
  let move_to (printer : printer) (position : position) : unit =
    while printer.position.line < position.line do
      print_char printer '\n'
    done;
    while printer.position.column < position.column do
      print_char printer ' '
    done

  let print_comment (printer : printer) (comment : Token.comment) : unit =
    move_to printer comment.span.start;
    print_str printer.fmt comment.shape.raw "grey";
    printer.position <- comment.span.finish

  let print_keyword printer (token : Token.Shape.t) =
    match Token.Shape.raw token with
    | Some raw -> print_str printer.fmt raw "magenta"
    | None -> ()

  let print_syntax_part printer (token : Token.Shape.t) =
    match Token.Shape.raw token with
    | Some raw -> print_str printer.fmt raw "yellow"
    | None -> ()

  let print_value printer (token : Token.Shape.t) =
    let fmt = printer.fmt in
    match token with
    | Ident { raw; _ } -> print_str fmt raw "white"
    | String { raw; _ } -> print_str fmt raw "green"
    | Number { raw; _ } -> print_str_italic fmt raw "white"
    | Punct { raw; _ } -> print_str fmt raw "white"
    | Comment _ -> unreachable "<comment> value??"
    | Eof -> unreachable "<eof> value??"
end

module Common (Output : Output) = struct
  let print_token (printer : printer) (f : printer -> Token.Shape.t -> unit)
      (token : Token.t) : unit =
    Output.move_to printer token.span.start;
    f printer token.shape;
    printer.position <- token.span.finish

  let rec print_ast (printer : printer) (ast : Ast.t) : unit =
    match ast.shape with
    | Simple { comments_before; token } ->
        comments_before |> List.iter (Output.print_comment printer);
        print_token printer Output.print_value token
    | Complex { parts; _ } ->
        parts
        |> List.iter (function
             | Ast.Value ast -> print_ast printer ast
             | Ast.Keyword token ->
                 print_token printer Output.print_keyword token
             | Ast.Comment comment -> Output.print_comment printer comment)
    | Syntax { tokens; value_after; _ } -> (
        tokens |> List.iter (print_token printer Output.print_syntax_part);
        match value_after with
        | None -> ()
        | Some value -> print_ast printer value)
end

let print (module Output : Output) (fmt : formatter)
    ({ ast; trailing_comments; eof } : Parser.result) : unit =
  let printer = { fmt; position = Position.beginning } in
  Output.initialize printer;
  (match ast with
  | Some ast ->
      let module Print = Common (Output) in
      Print.print_ast printer ast
  | None -> ());
  trailing_comments
  |> List.iter (fun comment -> Output.print_comment printer comment);
  Output.move_to printer eof;
  Output.finalize printer
