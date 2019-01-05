{ pkgs ? import <nixpkgs> {} }: with pkgs;

(callPackage ./release.nix {}).env.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [ cabal-install ];
})
