{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        inherit system;
      };
      nix-filter = inputs.nix-filter.lib;
    in
    {
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = with pkgs; [
            just
            (pkgs.writeShellScriptBin "kast" ''
              echo TODO
              exit 1
            '')

            # for running js output
            nodejs

            # NIX
            nixfmt
            nil
          ];
          shellHook = ''
            echo Hello from Kast devshell
          '';
        };
      };
      formatter.${system} = pkgs.nixfmt;
    };
}
