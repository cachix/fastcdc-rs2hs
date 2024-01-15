{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    cargo-cabal.url = "github:yvan-sraka/cargo-cabal";
  };

  outputs = { nixpkgs, flake-utils, cargo-cabal, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.cabal-install
          pkgs.ghc
          pkgs.cargo
          pkgs.rustc
          cargo-cabal.defaultPackage.${system}
        ];
      };
      }
    );
}
