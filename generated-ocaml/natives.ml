let rng (max, min) = Random.int32_in_range ~min ~max

let input prompt =
  print_string prompt;
  read_line ()
;;

let print line = print_endline line
let eprint line = Printf.eprintf "%s" line
let todo () = failwith "todo"
