{ config ? {}
, sourcesOverride ? {}
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;

cardanoMqSyncProject.shellFor {
  name = "cabal-dev-shell";

  packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

  # These programs will be available inside the nix-shell.
  nativeBuildInputs = [
    nix-prefetch-git
    pkg-config
    hlint
    ghcid
    haskell-language-server
    cabalWrapped
    # we also add cabal (even if cabalWrapped will be used by default) for shell completion:
    cabal
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
