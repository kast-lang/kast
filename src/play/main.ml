open Format

let cond () = print_string "(a or b) and (c or d)"

let then_case () =
  for i = 0 to 5 do
    if i <> 0 then print_space ();
    print_string "print true"
  done

let else_case () =
  for i = 0 to 0 do
    if i <> 0 then print_space ();
    print_string "print true"
  done

let () =
  set_margin 40;
  open_vbox 0;
  print_string "if";
  print_space ();
  open_hvbox 0;
  print_string "condition";
  close_box ();
  close_box ();
  print_newline ()
