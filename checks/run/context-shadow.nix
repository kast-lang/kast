{ self, ... }:

{
  source = self.outPath + "/examples/context_shadow.ks";
  expected_output = builtins.toFile "expected_output" ''
    123
    shadow
  '';
}
