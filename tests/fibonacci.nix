{ ... }:

{
  import = ../examples/fibonacci.ks;
  source = builtins.toFile "test.ks" ''
    dbg (fib 10)
  '';
  expected_output = builtins.toFile "expected_output" ''
    89 :: int32
  '';
}
