{ self, ... }:

{
  source = self.outPath + "/examples/guess-a-number.ks";
  input = builtins.toFile "input" ''
    exit
  '';
  # TODO use a seed to actually play the game
  expected_output = builtins.toFile "expected_output" ''
    a number has been picked
    guess:
    quitted
  '';
}
