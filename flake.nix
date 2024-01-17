{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in {
      devShells.default = pkgs.mkShell {
        packages = [
          pkgs.cabal-install
          pkgs.ghc
          pkgs.cargo
          pkgs.pkg-config
          pkgs.cargo-c
          pkgs.rust-bin.nightly.latest.default
          pkgs.rust-analyzer
        ];

        shellHook = ''
          export PKG_CONFIG_PATH=$PWD/target/lib/pkgconfig
          export LD_LIBRARY_PATH=$PWD/target/lib
        '';
      };
      }
    );
}
