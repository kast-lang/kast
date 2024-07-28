{ ... }:

{
  source = ../examples/generator.ks;
  expected_output = builtins.toFile "expected_output" ''
    yielding 1
    1
    yielding 2
    2
    now using generator as value
    calling .next()
    yielding 1
    Yielded "1" :: Finished of void | Yielded of string
    calling .next()
    yielding 2
    Yielded "2" :: Finished of void | Yielded of string
    calling .next()
    Finished void :: Finished of void | Yielded of string
  '';
}
