{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      genetic-algorithm = self.callPackage ./default.nix {};
    };
  };

  haskellDeps = ps: with ps; [
    base
    random
    mtl
    genetic-algorithm
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    haskellPackages.genetic-algorithm
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
