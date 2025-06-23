open Std

module Tuple = struct
  type 'a tuple = {
    unnamed : 'a array;
    named : 'a StringMap.t;
    named_order_rev : string list;
  }

  type 'a t = 'a tuple

  type member =
    | Index of int
    | Name of string

  let get_unnamed index tuple = Array.get tuple.unnamed index
  let get_named name tuple = StringMap.find name tuple.named

  let get member tuple =
    match member with
    | Index index -> get_unnamed index tuple
    | Name name -> get_named name tuple

  let zip_order_a : 'a 'b. 'a tuple -> 'b tuple -> ('a * 'b) tuple =
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

  let to_seq : 'a. 'a tuple -> (member * 'a) Seq.t =
   fun { unnamed; named; named_order_rev } ->
    Seq.append
      (unnamed |> Array.to_seq |> Seq.mapi (fun i x -> (Index i, x)))
      (named_order_rev |> List.rev |> List.to_seq
      |> Seq.map (fun name -> (Name name, StringMap.find name named)))

  let empty : 'a. 'a tuple =
    { unnamed = [||]; named = StringMap.empty; named_order_rev = [] }

  let add : 'a. string option -> 'a -> 'a tuple -> 'a tuple =
   fun (type a) (name : string option) (value : a) (tuple : a tuple) :
       a tuple ->
    match name with
    | Some name ->
        {
          tuple with
          named = StringMap.add name value tuple.named;
          named_order_rev = name :: tuple.named_order_rev;
        }
    | None ->
        { tuple with unnamed = Array.append tuple.unnamed (Array.make 1 value) }

  let make : 'a. 'a list -> (string * 'a) list -> 'a tuple =
   fun unnamed named ->
    let tuple_of_unnamed =
      List.fold_left (fun tuple value -> add None value tuple) empty unnamed
    in
    List.fold_left
      (fun tuple (name, value) -> add (Some name) value tuple)
      tuple_of_unnamed named

  let merge : 'a. 'a tuple -> 'a tuple -> 'a tuple =
   fun a b ->
    let unnamed = Array.append a.unnamed b.unnamed in
    let named_order_rev = List.append b.named_order_rev a.named_order_rev in
    let named =
      StringMap.union
        (fun _name _a _b -> invalid_arg "Tuple.merge")
        a.named b.named
    in
    { unnamed; named; named_order_rev }

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a tuple -> unit =
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

type 'a tuple = 'a Tuple.tuple
