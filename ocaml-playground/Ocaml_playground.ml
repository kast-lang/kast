let report_memory_usage =
  let last_print = ref 0. in
  let print_interval = 10. in
  fun () ->
    let current_time = Unix.time () in
    let pid = Unix.getpid () in
    if !last_print +. print_interval < current_time then
      Printf.ksprintf
        (fun cmd -> ignore (Sys.command cmd))
        "grep -i rss /proc/%d/status" pid

let make_counter () =
  let count = ref 0 in
  let inc = fun () -> count := !count + 1 in
  let dec = fun () -> count := !count - 1 in
  let get = fun () -> !count in
  (get, inc, dec)

let _ =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let mutex = Eio.Mutex.create () in
          let i = ref 0 in
          let stdin =
            Eio.Buf_read.of_flow ~max_size:1_000_000 (Eio.Stdenv.stdin env)
          in
          let stdout = Eio.Stdenv.stdout env in
          let get, inc, dec = make_counter () in
          while true do
            let _ : string = Eio.Buf_read.line stdin in
            Eio.Fiber.fork ~sw (fun () ->
                inc ();
                Eio.Mutex.use_rw ~protect:false mutex (fun () ->
                    Eio.Flow.copy_string "hello, world!" stdout;
                    dec ());
                ());
            if !i mod 1 = 0 then (
              Format.eprintf "spawned so far: %d\ncount=%d\n" !i (get ());
              report_memory_usage ();
              flush Stdlib.stderr);
            i := !i + 1
          done))
