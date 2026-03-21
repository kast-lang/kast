{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast.url = "github:kast-lang/kast";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
        kast = inputs.kast.packages.${system}.default;
      in with pkgs; {
        devShells.default = mkShell {
          packages = [
            (pkgs.writeShellScriptBin "kast" ''
              systemd-run --quiet --user --scope -p MemoryMax=5G \
                rlwrap ${kast}/bin/kast "$@"
            '')
            (pkgs.writeShellScriptBin "self-kast" ''
              systemd-run --quiet --user --scope -p MemoryMax=5G \
                rlwrap kast $SELF_KAST_ARGS src/main.ks "$@"
            '')
            rlwrap
            nixfmt-classic
            nodejs
            just
            fd
          ];
          shellHook = ''
            echo Hello from Kast dev shell
            export SELF_KAST_ARGS="--use-numbers-instead-of-symbols false --quiet --target js"
          '';
        };
      });
}
