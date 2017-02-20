{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./monad-batcher-example.nix {
    monad-batcher = pkgs.haskell.lib.buildStrictly
                      (haskellPackages.callPackage ../monad-batcher.nix {});
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
