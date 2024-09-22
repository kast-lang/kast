{ self, ... }:

{
  source = self.outPath + "/examples/generator.ks";
  expected_output = builtins.toFile "expected_output" ''
    yielding 1
    1
    yielding 2
    2
    now using generator as value
    calling .next()
    yielding 1
    .Yielded "1" :: .Finished void | .Yielded string
    calling .next()
    yielding 2
    .Yielded "2" :: .Finished void | .Yielded string
    calling .next()
    .Finished void :: .Finished void | .Yielded string
  '';
}
