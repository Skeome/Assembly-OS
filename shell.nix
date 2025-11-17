{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    nasm
    gcc
    SDL2
    pkg-config
    gdb
  ];

  shellHook = ''
    echo "Welcome to your Assembly Development Shell!"
    echo "Available tools: nasm, gcc, SDL2, pkg-config, gdb"
  '';
}
