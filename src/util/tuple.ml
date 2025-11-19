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
  let get_named_opt name tuple = StringMap.find_opt name tuple.named

  let get member tuple =
    match member with
    | Index index -> get_unnamed index tuple
    | Name name -> get_named name tuple

  let is_unnamed len tuple =
    tuple.named_order_rev |> List.length = 0
    && tuple.unnamed |> Array.length = len

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

  let of_list : 'a. (string option * 'a) list -> 'a tuple =
   fun list ->
    List.fold_left (fun tuple (name, value) -> add name value tuple) empty list

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

  type print_options = {
    named_field_before : string;
    named_field_middle : string;
    field_sep : string;
    open_ : string;
    close : string;
  }

  let default_print_options : print_options =
    {
      open_ = "(";
      field_sep = ",";
      named_field_before = ".";
      named_field_middle = " = ";
      close = ")";
    }

  let print :
      'a.
      ?options:print_options ->
      (formatter -> 'a -> unit) ->
      formatter ->
      'a tuple ->
      unit =
   fun ?(options = default_print_options) print_value fmt
       { unnamed; named; named_order_rev } ->
    Format.pp_print_custom_break fmt ~fits:(options.open_, 1, "")
      ~breaks:(options.open_, 2, "");
    let sep fmt () =
      Format.pp_print_custom_break fmt ~fits:(options.field_sep, 1, "")
        ~breaks:(options.field_sep, 2, "")
    in
    let print_unnamed fmt value = fprintf fmt "@[<v>%a@]" print_value value in
    Format.pp_print_iter ~pp_sep:sep Array.iter print_unnamed fmt unnamed;
    let has_unnamed = Array.length unnamed <> 0 in
    let has_named = not (StringMap.is_empty named) in
    if has_unnamed && has_named then sep fmt ();
    let print_named fmt name =
      let value = StringMap.find name named in
      fprintf fmt "@[<v>%s%a%s%a@]" options.named_field_before
        String.print_maybe_escaped name options.named_field_middle print_value
        value
    in
    Format.pp_print_iter ~pp_sep:sep List.iter print_named fmt
      (List.rev named_order_rev);
    Format.pp_print_custom_break fmt ~fits:("", 1, options.close)
      ~breaks:
        ( (if has_named || has_unnamed then options.field_sep else ""),
          0,
          options.close )

  let map : 'a 'b. ('a -> 'b) -> 'a tuple -> 'b tuple =
   fun f { unnamed; named; named_order_rev } ->
    {
      unnamed = unnamed |> Array.map f;
      named_order_rev;
      named =
        named_order_rev |> List.rev
        |> List.map (fun name ->
            let current = StringMap.find name named in
            (name, f current))
        |> StringMap.of_list;
    }

  let mapi : 'a 'b. (member -> 'a -> 'b) -> 'a tuple -> 'b tuple =
   fun f { unnamed; named; named_order_rev } ->
    {
      unnamed = unnamed |> Array.mapi (fun i -> f (Index i));
      named_order_rev;
      named =
        named_order_rev |> List.rev
        |> List.map (fun name ->
            let current = StringMap.find name named in
            (name, f (Name name) current))
        |> StringMap.of_list;
    }

  let unwrap : 'a. unnamed:int -> named:string list -> 'a tuple -> 'a list =
   fun ~unnamed ~named tuple ->
    let actual_unnamed = tuple.unnamed |> Array.length in
    if actual_unnamed <> unnamed then
      fail "Expected %d unnamed fields, got %d" unnamed actual_unnamed;
    let named_set = StringSet.of_list named in
    let actual_unnamed = tuple.named_order_rev |> StringSet.of_list in
    if StringSet.equal named_set actual_unnamed |> not then
      fail "Expected named fields %a, got %a"
        (List.print String.print_dbg)
        named
        (List.print String.print_dbg)
        (tuple.named_order_rev |> List.rev);
    List.append
      (tuple.unnamed |> Array.to_list)
      (named |> List.map (fun name -> StringMap.find name tuple.named))

  let assert_empty : 'a. 'a tuple -> unit =
   fun tuple -> ignore <| unwrap ~unnamed:0 ~named:[] tuple

  let unwrap_single_unnamed : 'a. 'a tuple -> 'a =
   fun tuple ->
    match unwrap ~unnamed:1 ~named:[] tuple with
    | [ a ] -> a
    | _ -> unreachable "Tuple.unwrap_single_unnamed"

  let unwrap_single_named : 'a. string -> 'a tuple -> 'a =
   fun name tuple ->
    match unwrap ~unnamed:0 ~named:[ name ] tuple with
    | [ a ] -> a
    | _ -> unreachable "Tuple.unwrap_single_unnamed"

  let unwrap2 : 'a. unnamed:int -> named:string list -> 'a tuple -> 'a * 'a =
   fun ~unnamed ~named tuple ->
    match unwrap ~unnamed ~named tuple with
    | [ a; b ] -> (a, b)
    | _ -> invalid_arg "Tuple.unwrap2"

  let unwrap_unnamed2 tuple = unwrap2 ~unnamed:2 ~named:[] tuple
  let unwrap_named2 names tuple = unwrap2 ~unnamed:0 ~named:names tuple

  let unwrap3 : 'a. unnamed:int -> named:string list -> 'a tuple -> 'a * 'a * 'a
      =
   fun ~unnamed ~named tuple ->
    match unwrap ~unnamed ~named tuple with
    | [ a; b; c ] -> (a, b, c)
    | _ -> invalid_arg "Tuple.unwrap3"

  let unwrap_unnamed3 tuple = unwrap3 ~unnamed:3 ~named:[] tuple
  let unwrap_named3 names tuple = unwrap3 ~unnamed:0 ~named:names tuple
end

type 'a tuple = 'a Tuple.tuple
