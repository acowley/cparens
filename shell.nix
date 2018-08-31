{ compiler ? "ghc843"
, withHoogle ? true
}:
let
pkgs = import <nixpkgs> {};
packageSet = pkgs.haskell.packages.${compiler};
hspkgs =
  if withHoogle then
    packageSet.override {
        overrides = (self: super: {
          ghc = super.ghc // {
            withPackages = f: super.ghc.withHoogle (ps:
              f ps ++ [ps.intero ps.cabal-install]
            );
          };
          intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          ghcWithPackages = self.ghc.withPackages;
        });
      }
  else packageSet;
drv = hspkgs.callPackage (import ./default.nix) {};
in
if pkgs.lib.inNixShell then drv.env else drv
