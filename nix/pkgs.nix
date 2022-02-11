# our packages overlay
final: prev: with final;
  let
    compiler-nix-name = config.haskellNix.compiler or "ghc8105";
  in {
  cardanoMqSyncProject = import ./haskell.nix {
    name = "cardano-node";
    inherit compiler-nix-name
      lib
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoMqSyncHaskellPackages = cardanoMqSyncProject.hsPkgs;

  #Grab the executable component of our package.
  inherit (cardanoMqSyncHaskellPackages.cardano-mq-sync.components.exes) cardano-mq-sync;

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (cardanoMqSyncProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (cardanoMqSyncProject) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (cardanoMqSyncProject) index-state;
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "latest";
    inherit (cardanoMqSyncProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (cardanoMqSyncProject) index-state;
  };
}
