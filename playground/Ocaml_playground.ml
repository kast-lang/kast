let () =
  let rec pow acc x n = if n = 0 then 1 else acc * x * pow 1 x (n - 1) in
  let x = pow 1 10 60 in
  print_int x
;;
