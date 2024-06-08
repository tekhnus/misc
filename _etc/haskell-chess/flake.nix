{
  description = "Haskell chess";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
        };
        haskellEnv = pkgs.haskellPackages.ghcWithPackages (ps: [ ps.stack ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ haskellEnv ];
        };
      }
    );
}

