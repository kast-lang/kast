{ ... }:

{
  source = builtins.toFile "test.ks" ''
    print "hello, world!"
  '';
  expected_output = builtins.toFile "expected_output" "hello, world!\n";
}
