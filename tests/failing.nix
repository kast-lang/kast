{ ... }:

{
  source = builtins.toFile "src" ''
    std.panic "Poop"
  '';
  expected_output = builtins.toFile "expected_output" ''
  '';
  expected_exit_code = 2;
}
