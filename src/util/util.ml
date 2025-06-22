open Std

module Position = struct
  type t = { index : int; line : int; column : int }
  type position = t

  let beginning : position = { index = 0; line = 1; column = 1 }

  let advance : char -> position -> position =
   fun c p ->
    match c with
    | '\n' -> { index = p.index + 1; line = p.line + 1; column = 1 }
    | _ -> { index = p.index + 1; line = p.line; column = p.column + 1 }

  let print : 'a. formatter -> position -> unit =
   fun fmt { index = _; line; column } -> fprintf fmt "%d:%d" line column
end

type position = Position.t

module Source = struct
  type t = { contents : string; filename : string }
end

type source = Source.t

module Span = struct
  type t = { start : position; finish : position; filename : string }
  type span = t

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; filename } ->
    fprintf fmt "%s:%d.%d-%d.%d" filename start.line start.column finish.line
      finish.column
end

type span = Span.t

module Spanned = struct
  type 'a t = { value : 'a; span : span }
  type 'a spanned = 'a t

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a spanned -> unit =
   fun print_value fmt spanned ->
    fprintf fmt "%a at %a" print_value spanned.value Span.print spanned.span
end

type 'a spanned = 'a Spanned.t

module Tuple = struct
  module StringMap = Map.Make (String)

  type 'a t = {
    unnamed : 'a array;
    named : 'a StringMap.t;
    named_order_rev : string list;
  }

  type member = Index of int | Name of string

  let zip_order_a : 'a 'b. 'a t -> 'b t -> ('a * 'b) t =
   fun a b ->
    let unnamed =
      try Array.combine a.unnamed b.unnamed
      with Invalid_argument _ -> invalid_arg "Tuple.zip (unnamed)"
    in
    let named_order_rev = a.named_order_rev in
    let named =
      try
        named_order_rev
        |> List.map (fun name ->
               (name, (StringMap.find name a.named, StringMap.find name b.named)))
        |> StringMap.of_list
      with Not_found -> invalid_arg "Tuple.zip (named)"
    in
    { unnamed; named; named_order_rev }

  let to_seq : 'a. 'a t -> (member * 'a) Seq.t =
   fun { unnamed; named; named_order_rev } ->
    Seq.append
      (unnamed |> Array.to_seq |> Seq.mapi (fun i x -> (Index i, x)))
      (named_order_rev |> List.rev |> List.to_seq
      |> Seq.map (fun name -> (Name name, StringMap.find name named)))

  let empty : 'a. 'a t =
    { unnamed = [||]; named = StringMap.empty; named_order_rev = [] }

  let add : 'a. string option -> 'a -> 'a t -> 'a t =
   fun (type a) (name : string option) (value : a) (tuple : a t) : a t ->
    match name with
    | Some name ->
        {
          tuple with
          named = StringMap.add name value tuple.named;
          named_order_rev = name :: tuple.named_order_rev;
        }
    | None ->
        { tuple with unnamed = Array.append tuple.unnamed (Array.make 1 value) }

  let make : 'a. 'a list -> (string * 'a) list -> 'a t =
   fun unnamed named ->
    let tuple_of_unnamed =
      List.fold_left (fun tuple value -> add None value tuple) empty unnamed
    in
    List.fold_left
      (fun tuple (name, value) -> add (Some name) value tuple)
      tuple_of_unnamed named

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a t -> unit =
   fun print_value fmt { unnamed; named; named_order_rev } ->
    Format.pp_print_custom_break fmt ~fits:("(", 1, "") ~breaks:("(", 2, "");
    let comma fmt () =
      Format.pp_print_custom_break fmt ~fits:(",", 1, "") ~breaks:(",", 2, "")
    in
    let print_unnamed fmt value = fprintf fmt "@[<v>%a@]" print_value value in
    Format.pp_print_iter ~pp_sep:comma Array.iter print_unnamed fmt unnamed;
    if Array.length unnamed <> 0 && StringMap.cardinal named <> 0 then
      comma fmt ();
    let print_named fmt name =
      let value = StringMap.find name named in
      fprintf fmt "@[<v>%S = %a@]" name print_value value
    in
    Format.pp_print_iter ~pp_sep:comma List.iter print_named fmt
      (List.rev named_order_rev);
    Format.pp_print_custom_break fmt ~fits:("", 1, ")") ~breaks:(",", 0, ")")
end

type 'a tuple = 'a Tuple.t
