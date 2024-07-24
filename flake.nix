{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.${system}.devenv-up = self.devShells.${system}.default.config.procfileScript;
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
                  pwd;
                  set > vars;
                  echo \$TMP;
                  dune build -w;
                "
          '';
        };
      };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
