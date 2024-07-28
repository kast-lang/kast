{ ... }:
{
  source = ../examples/exceptions.ks;
  expected_output = builtins.toFile "expected_output" ''
    1 :: int32
    thrown
    hello
  '';
}
