{
  description = "Hardgame";

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
        pythonEnv = pkgs.python3.withPackages (ps: [ ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pythonEnv ];
        };
      }
    );
}

