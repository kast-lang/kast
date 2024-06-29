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
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; with ocamlPackages; [
          # opam
          dune_3
          ocaml
          ocaml-lsp
          ocamlformat
          just
        ];
        # shellHook = ''
        #   eval $(opam env)
        # '';
      };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
