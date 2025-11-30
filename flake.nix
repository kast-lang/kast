{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs = {
      nixpkgs.follows = "nixpkgs";
      opam-repository.follows = "opam-repository";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    let package = "kast";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        opam-nix = inputs.opam-nix.lib.${system};
        devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          ocaml-index = "*";
          dune-deps = "*";
          odoc = "*";
          odig = "*";
          utop = "*";
        };
        query = devPackagesQuery // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-base-compiler = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };
        overlays = [
          (final: prev: {
            ppxlib = let
              src = builtins.fetchGit {
                url = "https://github.com/NathanReb/ppxlib";
                rev = "7fa47adcba0261acf6aa39736a9c7d80a70815c7";
              };
            in opam-nix.buildOpamProject { } "ppxlib" src {
              ocaml-base-compiler = "*";
            };
          })
        ];
        scope = opam-nix.buildDuneProject { } package ./. query;
        overlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (_: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
          });
          ppxlib = let
            src = builtins.fetchGit {
              url = "https://github.com/NathanReb/ppxlib";
              rev = "7fa47adcba0261acf6aa39736a9c7d80a70815c7";
            };
          in (opam-nix.buildOpamProject { } "ppxlib" src {
            ocaml-base-compiler = "*";
          }).ppxlib;
        };
        scope' = scope.overrideScope overlay;
        # The main package containing the executable
        main = scope'.${package};
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in {
        legacyPackages = scope';
        packages.default = main.overrideAttrs {
          buildInputs = [ pkgs.makeWrapper ];
          postFixup = ''
            wrapProgram $out/bin/kast --set KAST_STD ${./std}
          '';
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          buildInputs = devPackages ++ (with pkgs; [
            (pkgs.writeShellScriptBin "kast" ''
              systemd-run --user --scope -p MemoryMax=1G \
                rlwrap dune exec kast -- "$@"
            '')
            # You can add packages from nixpkgs here
            just # look at .justfile
            nodejs # for running js output
            graphviz # for dep-graph
            caddy # serve html
            nixfmt-classic # nix formatter
            nil # nix lsp
            rlwrap
          ]);
          shellHook = ''
            echo 'Hello from Kast devshell'
            echo '  dont forget to run `just lsp-support` :)'
            export OCAML_BACKTRACE=1
            export OCAMLRUNPARAM=b
            export DUNE_CONFIG__GLOBAL_LOCK=disabled
          '';
        };
        formatter = pkgs.nixfmt-classic;
      });
}
