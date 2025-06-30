open Format

module List = struct
  include Stdlib.List

  let zip : 'a. 'a list -> 'b list -> ('a * 'b) list = combine

  let format_options : Format.Iter.options =
    {
      before = { fits = ("[", 1, ""); breaks = ("[", 2, "") };
      sep = { fits = (",", 1, ""); breaks = (",", 2, "") };
      after = { fits = ("", 1, "]"); breaks = (",", 0, "]") };
    }

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun print_value fmt list ->
    let print_value fmt value = fprintf fmt "@[<v>%a@]" print_value value in
    Format.Iter.print format_options iter print_value fmt list

  let tail = Stdlib.List.tl
  let head = Stdlib.List.hd
  let last list = list |> rev |> head

  let sort_by_key compare_key get_key =
    sort (fun a b -> compare_key (get_key a) (get_key b))
end
