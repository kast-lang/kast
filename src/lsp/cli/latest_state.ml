open Std

type 'a state = {
  sw : Eio.Switch.t;
  mutable result : 'a option;
  mutable currently_executing : unit Eio.Promise.or_exn option;
  mutable queued : (unit -> 'a) option;
}

type 'a t = 'a state

let init ~sw = { sw; result = None; currently_executing = None; queued = None }

let rec start_queued_if_needed : 'a. domain_mgr:_ -> 'a state -> unit =
 fun (type a) ~domain_mgr (state : a state) : unit ->
  with_return (fun { return } ->
      if state.currently_executing |> Option.is_some then return ();
      let f = state.queued |> Option.unwrap_or_else (fun () -> return ()) in
      state.queued <- None;
      state.currently_executing <-
        Some
          (Eio.Fiber.fork_promise ~sw:state.sw (fun () ->
               Eio.Domain_manager.run domain_mgr (fun () ->
                   Kast.handle_effects (fun () ->
                       try
                         Fun.protect
                           ~finally:(fun () ->
                             state.currently_executing <- None)
                           (fun () -> state.result <- Some (f ()))
                       with exc ->
                         Log.error (fun log ->
                             log "DOMAIN PANIC: %s" (Printexc.to_string exc))));
               start_queued_if_needed ~domain_mgr state)))

let spawn (type a) ~domain_mgr (f : unit -> a) (state : a state) : unit =
  state.queued <- Some f;
  start_queued_if_needed ~domain_mgr state

let rec get_latest_result : 'a. 'a state -> 'a option =
 fun (type a) (state : a state) : a option ->
  match state.currently_executing with
  | Some promise ->
      Eio.Promise.await_exn promise;
      (* maybe another one has now started *)
      get_latest_result state
  | None -> state.result
