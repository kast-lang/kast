{ kast, system, pkgs, self, ... }:

let
  checkAst =
    builtins.mapAttrs
      (name: _entry:
        let
          test = import ./ast/${name} { inherit pkgs self; };
          expected_exit_code = builtins.toString (test.expected_exit_code or 0);
        in
        pkgs.stdenv.mkDerivation {
          name = "kast-check-ast-${name}";
          nativeBuildInputs = [ kast ];
          dontUnpack = true;
          # doCheck = true;
          buildPhase = ''
            set +e
            kast parse-ast < ${test.source} > $out
            EXIT_CODE=''$?
            set -e
            if [ "''$EXIT_CODE" -ne ${expected_exit_code} ]; then
              echo "Expected exit code ${expected_exit_code}, got ''$EXIT_CODE"
              exit 1
            fi
            diff $out ${test.expected_output}
          '';
        }
      )
      (builtins.readDir ./ast);
  runTests =
    builtins.mapAttrs
      (name: _entry:
        let
          test = import ./run/${name} { inherit pkgs self; };
          expected_exit_code = builtins.toString (test.expected_exit_code or 0);
        in
        pkgs.stdenv.mkDerivation {
          name = "kast-check-run-${name}";
          nativeBuildInputs = [ kast pkgs.nodejs ];
          dontUnpack = true;
          # doCheck = true;
          buildPhase = ''
            set +e
            kast ${test.import or ""} ${test.source} < ${test.input or "/dev/null"} > $out
            EXIT_CODE=''$?
            set -e
            if [ "''$EXIT_CODE" -ne ${expected_exit_code} ]; then
              echo "Expected exit code ${expected_exit_code}, got ''$EXIT_CODE"
              exit 1
            fi
            diff $out ${test.expected_output}

            if ${if test.test-js or false then "true" else "false"}; then
              kast --to-js ${test.source} > compiled.js
              set +e
              node compiled.js < ${test.input or "/dev/null"} > $out
              EXIT_CODE=''$?
              set -e
              if [ "''$EXIT_CODE" -ne ${expected_exit_code} ]; then
                echo "Expected exit code ${expected_exit_code}, got ''$EXIT_CODE"
                exit 1
              fi
              diff $out ${test.expected_output}
            fi
          '';
        }
      )
      (builtins.readDir ./run);
in

checkAst
# // runTests

