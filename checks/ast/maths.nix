{ self, ... }:

{
  source = builtins.toFile "test.ks" ''
    a + sin x * c * d + (1 - 2)
  '';
  expected_output = builtins.toFile "expected_output" ''
    op binary + (
        lhs: op binary + (
            lhs: "a"
            rhs: op binary * (
                lhs: op binary * (
                    lhs: builtin macro call (
                        f: "sin"
                        arg: "x"
                    )
                    rhs: "c"
                )
                rhs: "d"
            )
        )
        rhs: builtin macro scope (
            e: op binary - (
                lhs: "1"
                rhs: "2"
            )
        )
    )
  '';
}
