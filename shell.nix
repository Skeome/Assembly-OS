{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    coreutils
    gdb
    nasm
    qemu
  ];

  shellHook = ''
    echo "Welcome to your Assembly Development Shell!"
    echo "Available tools: coreutils, gdb, nasm, qemu"
  '';
}
