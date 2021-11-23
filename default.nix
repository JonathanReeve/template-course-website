let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    name = "course-data-ethics";
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          haskell-language-server
          ghcid
        ]);
    source-overrides = {
      # plotlyhs = builtins.fetchTarball "https://github.com/JonathanReeve/plotlyhs/archive/0fcf833.tar.gz";
    };
  }
