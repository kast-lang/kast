{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Precisely filter files copied to the nix store
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, nix-filter }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.${system} = {
        default = with pkgs; ocamlPackages.buildDunePackage {
          pname = "kast";
          version = "0.1.0";
          duneVersion = "3";
          src = nix-filter.lib {
            root = ./.;
            include = [
              ".ocamlformat"
              "dune-project"
              (nix-filter.lib.inDirectory "bin")
              (nix-filter.lib.inDirectory "lib")
              (nix-filter.lib.inDirectory "test")
            ];
          };
          buildInputs = [
            # Ocaml package dependencies needed to build go here.
            makeWrapper
          ];
          strictDeps = true;
          preBuild = ''
            dune build kast.opam
          '';
          postFixup = ''
            wrapProgram $out/bin/kast --set KAST_STD ${./std}
          '';
        };
      };
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            haskell-language-server
            haskellPackages.hoogle
            cabal-install # haskell package manager
            ormolu # haskell formatter
            just
            rlwrap
            zola
            screen
            caddy
          ];
          shellHook = ''
            echo Hello from Kast devshell
            mkdir -p .flock
            mkdir -p .logs
            echo "These services should now be running (you can check with screen -ls):"
            screen -L -Logfile .logs/zola -S zola -dm \
              flock --conflict-exit-code 0 --nonblock .flock/zola \
                bash -c "cd website && zola serve"
            echo "  zola: serving the website at http://127.0.0.1:1111"
          '';
        };
      };
      formatter.${system} = pkgs.nixpkgs-fmt;
      checks.${system} = builtins.mapAttrs
        (name: _entry:
          let
            test = import ./tests/${name} { inherit pkgs; };
            expected_exit_code = builtins.toString (test.expected_exit_code or 0);
          in
          pkgs.stdenv.mkDerivation {
            name = "kast-check-${name}";
            nativeBuildInputs = [ self.packages.${system}.default pkgs.nodejs ];
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
        (builtins.readDir ./tests);
    };
}
