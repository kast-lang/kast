{ self, ... }:

{
  source = self.outPath + "/examples/rec-closures.ks";
  expected_output = builtins.toFile "expected_output" ''
    going in
    5 :: int32
    calling g
    4 :: int32
    calling f
    3 :: int32
    calling g
    2 :: int32
    calling f
    1 :: int32
    calling g
    0 :: int32
  '';
}
