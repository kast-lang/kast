let _ = print_endline "starting"
let big_list = List.init 1_000_000 (fun n -> n)

(* let _domain =
  Domain.spawn (fun () ->
      while true do
        let _ = Marshal.to_string big_list [] in
        ()
      done) *)

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

(* let rec loop chain =
  let _ = Marshal.to_string big_list [] in
  report_memory_usage ();
  loop chain *)

let _ =
  Eio_main.run (fun _env ->
      Eio.Switch.run (fun sw ->
          print_endline "+";
          let i = ref 0 in
          while true do
            Eio.Fiber.fork ~sw (fun () ->
                (* print_endline "fork begin"; *)
                let s = Marshal.to_string big_list [] in
                (* print_endline "fork end"; *)
                ());
            if !i mod 2 = 0 then (
              Format.printf "spawned so far: %d\n" !i;
              report_memory_usage ();
              flush stdout);
            i := !i + 1
          done))
