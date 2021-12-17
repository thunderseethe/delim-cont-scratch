{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107" }:
nixpkgs.haskell.packages.${compiler}.developPackage {
  root = ./.;
  withHoogle = true;
  overrides = self: super: {
    trifecta = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "trifecta" (../trifecta) {};
  };
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv (with nixpkgs.haskellPackages;
      [ cabal-install haskell-language-server hoogle ghc ]);
}
