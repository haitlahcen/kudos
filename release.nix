{ pkgs ? import <nixpkgs> {} }: with pkgs;

haskell.packages.ghc844.callPackage ./default.nix {}
