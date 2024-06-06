# sfgui.nix
{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.stdenv.mkDerivation {
  name = "SFGUI";

  src = pkgs.fetchFromGitHub {
    owner = "TankOS";
    repo = "SFGUI";
    rev = "8347159";
    sha256 = "sha256-31S4s8yken+XWZ/W1vqMskQXGO2/RMhmh6j+J2E6jHQ=";
  };

  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = [
    pkgs.sfml
  ]
  # SFGUI uses SFML/OpenGL.h, so it must
  # link OpenGL itself.
  ++ pkgs.lib.optionals
    pkgs.stdenv.isDarwin
    [
      pkgs.darwin.apple_sdk.frameworks.OpenGL
    ]
  ++ pkgs.lib.optionals
    pkgs.stdenv.isLinux
    [
      pkgs.libGL
    ];

  cmakeFlags = [
    "-DSFGUI_BUILD_EXAMPLES=OFF"
  ];

  buildPhase = ''
    cmake .
    make
  '';
}

