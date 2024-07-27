{ ... }:

{
  source = ../examples/guess-a-number.ks;
  expected_output = builtins.toFile "expected_output" ''
  '';
}
