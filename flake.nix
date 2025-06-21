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
          packages =
            let
              ocamlPackages = with pkgs.ocamlPackages; [
                ocaml
                ocaml-lsp
                dune_3
                findlib
                utop
                odoc
                ocamlformat
              ];
              otherPackages = with pkgs; [

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
            in
            ocamlPackages ++ otherPackages;
          shellHook = ''
            echo Hello from Kast devshell
          '';
        };
      };
      formatter.${system} = pkgs.nixfmt;
    };
}
