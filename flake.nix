{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane-flake.url = "github:ipetkov/crane";

    # Precisely filter files copied to the nix store
    nix-filter-flake.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, nix-filter-flake, rust-overlay, crane-flake }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; overlays = [ (import rust-overlay) ]; };
      crane = crane-flake.mkLib pkgs;
      nix-filter = nix-filter-flake.lib;
      rust-toolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" ];
        targets = [ "wasm32-unknown-unknown" ];
      };
      kast =
        let
          commonArgs = {
            src = nix-filter {
              root = ./.;
              include = [
                "crates"
                "std"
                "src"
                "Cargo.toml"
                "Cargo.lock"
              ];
            };
          };
          cargoArtifacts = crane.buildDepsOnly commonArgs;
        in
        crane.buildPackage (commonArgs // {
          inherit cargoArtifacts;
          buildInputs = [
            pkgs.makeWrapper
          ];
          postFixup = ''
            wrapProgram $out/bin/kast --set KAST_STD ${./std}
          '';
        });
    in
    {
      packages.${system} = {
        default = kast;
      };
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = with pkgs; [
            rust-toolchain
            rust-analyzer
            just
            cargo-flamegraph
            cargo-outdated
          ];
          shellHook = ''
            echo Hello from Kast devshell
          '';
        };
      };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
