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
      packages.${system}.default = with pkgs; ocamlPackages.buildDunePackage {
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
        ];
        strictDeps = true;
        preBuild = ''
          dune build kast.opam
        '';
      };
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = with pkgs; with ocamlPackages; [
            # opam
            dune_3
            ocaml
            ocaml-lsp
            ocamlformat
            just
            rlwrap
            zola
            screen
          ];
          shellHook = ''
            echo Hello from Kast devshell
            mkdir -p .flock
            screen -S zola -dm \
              flock --conflict-exit-code 0 --nonblock .flock/zola \
                bash -c "cd website && zola serve"
            screen -S dune -dm \
              flock --conflict-exit-code 0 --nonblock .flock/dune \
                bash -c \
                "
                  unset TMP;
                  unset TMPDIR;
                  unset TEMP;
                  unset TEMPDIR;
                  unset NIX_BUILD_TOP;
                  dune build -w;
                "
          '';
        };
      };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
