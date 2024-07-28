{ ... }:

{
  source = ../examples/sum-types.ks;
  expected_output = builtins.toFile "expected_output" ''
    hello
    left
    right value
    unwrapped successfully
  '';
  expected_exit_code = 2;
}
