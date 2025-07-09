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
      Log.info "SPAWNING";
      state.currently_executing <-
        Some
          (Eio.Fiber.fork_promise ~sw:state.sw (fun () ->
               let result =
                 Eio.Domain_manager.run domain_mgr (fun () ->
                     Kast.handle_effects (fun () ->
                         try
                           Fun.protect
                             ~finally:(fun () ->
                               Log.info "END OF SPAWN";
                               state.currently_executing <- None;
                               Log.info "END OF SPAWN 2")
                             (fun () -> state.result <- Some (f ()));
                           Log.info "Done inside domain";
                           123
                         with exc ->
                           Log.error "DOMAIN PANIC: %s" (Printexc.to_string exc);
                           raise exc))
               in
               Log.info "RESULT = %d" result;
               start_queued_if_needed ~domain_mgr state;
               Log.info "Domain Done"));
      Log.info "SPAWNED")

let spawn (type a) ~domain_mgr (f : unit -> a) (state : a state) : unit =
  state.queued <- Some f;
  start_queued_if_needed ~domain_mgr state

let get_latest_result (type a) (state : a state) : a option =
  (match state.currently_executing with
  | Some promise ->
      Log.info "WAIT";
      Eio.Promise.await_exn promise;
      Log.info "WAITED"
  | None -> Log.info "No need to wait");
  state.result

let spawn (type a) ~domain_mgr (f : unit -> a) (state : a state) : unit =
  state.result <- Some (f ())

let get_latest_result (type a) (state : a state) : a option = state.result
