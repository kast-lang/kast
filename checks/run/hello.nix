{ self, ... }:

{
  source = self.outPath + "/examples/hello.ks";
  test-js = true;
  expected_output = builtins.toFile "expected_output" ''
    Hello
    World
  '';
}
