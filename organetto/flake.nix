{
  description = "Organetto";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              sfgui = import ./sfgui.nix { inherit pkgs; };
            })
          ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.jack2
            pkgs.sfml
            pkgs.sfgui
          ];
        };
      }
    );
}

