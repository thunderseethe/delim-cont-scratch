{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [ ghc cabal-install cabal2nix ];
}
