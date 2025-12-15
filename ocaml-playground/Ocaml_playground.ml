let () =
  let path = "/home/kuvi/projects/doom" in
  let uri = Uri.make ~scheme:"file" ~host:"" ~path () in
  print_endline (Uri.to_string uri)
