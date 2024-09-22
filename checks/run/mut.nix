{ self, ... }:

{
  source = self.outPath + "/examples/mut.ks";
  expected_output = builtins.toFile "expected_output" ''
    hello
    world
  '';
}
