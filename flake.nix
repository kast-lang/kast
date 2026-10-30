{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast-bootstrap.url = "github:kast-lang/kast/bootstrap-ocaml";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
    tcc-overlay.url = "github:vesdev/tcc-overlay";
    tcc-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ inputs.tcc-overlay.overlays.default ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
        kast-bootstrap = inputs.kast-bootstrap.packages.${system}.default;
        filter = inputs.nix-filter.lib;
      in
      with pkgs; {
        packages = rec {
          kast-js = stdenv.mkDerivation {
            name = "kast-js";
            src = filter {
              root = ./.;
              include = [ ".justfile" "src" "deps" ];
            };
            buildInputs = [ kast-bootstrap just ];
            buildPhase = ''
              KAST_BIN=${kast-bootstrap}/bin/kast just build
            '';
            installPhase = ''
              mkdir $out
              cp target/kast.mjs $out/kast.mjs
            '';
          };
          kast = pkgs.writeShellApplication {
            name = "kast";
            runtimeInputs = [ nodejs ];
            text = ''
              KAST_PATH=${./kast_path} node ${kast-js}/kast.mjs "$@"
            '';
          };
          kast-path = ./kast_path;
          default = kast;
          raylib-web = stdenv.mkDerivation {
            name = "raylib-web";
            src = raylib.src;
            buildInputs = [ emscripten ];
            buildPhase = ''
              cd src
              make PLATFORM=PLATFORM_WEB -B
            '';
            installPhase = ''
              mkdir $out
              cp libraylib.web.a $out/libraylib.web.a
            '';
          };
        };
        devShells.default = mkShell {
          packages = [
            (pkgs.writeShellScriptBin "kast-bootstrap" ''
              export KAST_PATH=${kast-bootstrap}
              systemd-run --quiet --user --scope -p MemoryMax=20G \
                ${kast-bootstrap}/bin/kast "$@"
            '')
            (pkgs.writeShellScriptBin "record-logs" ''
              LOG_FILE="logs/$1_$(date +%H-%M-%S)"
              shift
              mkdir -p logs
              # only keep 10 most recent logs
              ls --sort time logs | tail --lines +11 | xargs --replace={} rm logs/{}
              if [ -t 0 ]; then
                # tee $LOG_FILE.stdin | 
                faketty \
                  "$@" \
                  > >(tee $LOG_FILE.stdout) \
                  2> >(tee $LOG_FILE.stderr >&2)
              else
                tee $LOG_FILE.stdin | "$@" \
                  > >(tee $LOG_FILE.stdout) \
                  2> >(tee $LOG_FILE.stderr >&2)
              fi
            '')
            (pkgs.writeShellScriptBin "kast" ''
              record-logs kast ./target/kast.exe "$@"
            '')
            faketty
            rlwrap
            nixfmt
            nodejs
            just
            fd
            inotify-tools
            hyperfine
            gcc
            gdb
            boehmgc
            raylib
            emscripten
            caddy
            valgrind
            libbacktrace
            libunwind
            clang-tools
            tcc
          ];
          shellHook = ''
            echo Hello from Kast dev shell
            export KAST_PATH=$(pwd)/kast_path

            cat >.clangd <<EOF
            CompileFlags:
              Add: [
                -isystem, .,
                -isystem, $KAST_BOOTSTRAP_PATH/c-runtime/include,
                -Wall, -Wextra, -Wpedantic,
                -DKAST_ALLOCATION_STATS, -DKAST_DEBUG
              ]
            EOF
          '';
          KAST_BOOTSTRAP_PATH = "${kast-bootstrap}";
          RAYLIB = "${raylib}";
          RAYLIB_WEB = "${inputs.self.packages.${system}.raylib-web}/libraylib.web.a";
          CFLAGS = ''
            -Wfatal-errors
            -Wmissing-field-initializers
            -O1
            -lbacktrace -lunwind -g -rdynamic
            -fsanitize=address,undefined,leak
            -pthread
            -DKAST_DEBUG
          '';
        };
      });
}
