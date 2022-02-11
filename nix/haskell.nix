############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, haskell-nix
, buildPackages
# GHC attribute name
, compiler-nix-name
# Enable profiling
, profiling ? false
# Link with -eventlog
, eventlog ? false
# Enable asserts for given packages
, assertedPackages ? []
# Version info, to be passed when not building from a git work tree
, gitrev ? null
, name
}:
let

  src = (haskell-nix.haskellLib.cleanGit {
    name = "cardano-mq-sync-src";
    src = ../.;
  });

  rawProject = haskell-nix.cabalProject' (mkProjectArgs []);

  projectPackages =  lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    rawProject.hsPkgs);

  # It is important this common options matches in both calls to cabalProject or `cabal configure`
  # will run twice.
  mkProjectArgs = modules: {pkgs, ...}: {
    inherit compiler-nix-name  src modules name;
    cabalProjectLocal = ''
      allow-newer: terminfo:base
    '';
  };

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject' (mkProjectArgs [
    ({ pkgs, ...}: {
      # Use the VRF fork of libsodium
      packages = lib.genAttrs [ "cardano-crypto-praos" "cardano-crypto-class" ] (_: {
        components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      });
    })
    ({ pkgs, options, ...}: {
      # make sure that libsodium DLLs are available for windows binaries,
      # stamp executables with the git revision, add shell completion, strip/rewrite:
      packages = lib.genAttrs projectPackages (name: {
        # For checks:
        postInstall = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ''
          if [ -d $out/bin ]; then
            ${setLibSodium pkgs.libsodium-vrf}
          fi
        '';
      });
    })
  ]);

  setLibSodium = libsodium-vrf: "ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
in
  pkgSet // {
    inherit src projectPackages;
  }
