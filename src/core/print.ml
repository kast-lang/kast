open Std

module type T = sig
  type t
end

module Make (T : T) = struct
  type print_fn = T.t -> (formatter -> unit) option

  let printers : print_fn list Atomic.t = Atomic.make []

  let register_print : print_fn -> unit =
   fun f -> Atomic.set printers (f :: Atomic.get printers)

  let print : formatter -> T.t -> unit =
   fun fmt value ->
    let f =
      Atomic.get printers
      |> List.find_map (fun f -> f value)
      |> Option.unwrap_or_else (fun () -> failwith __LOC__)
    in
    f fmt
end

module type GadT = sig
  type _ t
end

module MakeGadt (T : GadT) = struct
  type print_fn = { f : 'a. 'a T.t -> (formatter -> unit) option }

  let printers : print_fn list Atomic.t = Atomic.make []

  let register_print : print_fn -> unit =
   fun f -> Atomic.set printers (f :: Atomic.get printers)

  let print : 'a. formatter -> 'a T.t -> unit =
   fun fmt value ->
    let f =
      Atomic.get printers
      |> List.find_map (fun f -> f.f value)
      |> Option.unwrap_or_else (fun () -> failwith __LOC__)
    in
    f fmt
end
