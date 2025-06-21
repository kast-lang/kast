{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs { inherit system; };
      nix-filter = inputs.nix-filter.lib;
      ocaml-index = pkgs.ocamlPackages.merlin.overrideAttrs (old: rec {
        pname = "ocaml-index";
        buildPhase = ''
          runHook preBuild
          dune build -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
          runHook postBuild
        '';
        installPhase = ''
          runHook preInstall
          dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname} \
            --docdir $out/share/doc --mandir $out/share/man
          runHook postInstall
        '';
      });
    in {
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = let
            ocamlPackages = with pkgs.ocamlPackages; [
              ocaml
              ocaml-lsp
              ocaml-index
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
          in ocamlPackages ++ otherPackages;
          shellHook = ''
            echo Hello from Kast devshell
            export OCAML_BACKTRACE=1
            export DUNE_CONFIG__GLOBAL_LOCK=disabled
          '';
        };
      };
      formatter.${system} = pkgs.nixfmt;
    };
}
