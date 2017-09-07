{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  cryptopals-hs = import ./default.nix {};
in
  if pkgs.lib.inNixShell then cryptopals-hs.env else cryptopals-hs
