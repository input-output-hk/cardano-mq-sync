{
  description = "Cardano MQ Sync";

  inputs = {
    # IMPORTANT: report any change to nixpkgs channel in nix/default.nix:
    nixpkgs.follows = "haskellNix/nixpkgs-2105";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix }:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      defaultSystem = "x86_64-linux";

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.utils
        (import ./nix/pkgs.nix)
      ];

    in eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.cardanoMqSyncProject.flake {
        };

        checks =
          # Linux only checks:
          optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) pkgs.nixosTests)
          )
          # checks run on default system only;
          // optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (pkgs.cardanoMqSyncProject.projectModule) src;
            };
          };

        exes = collectExes
           flake.packages;


        packages = {
          inherit (pkgs) cardano-mq-sync;
        }
        // exes

        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" checks);

      in recursiveUpdate flake {

        inherit packages checks devShell;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-mq-sync:exe:cardano-mq-sync";

        # Run by `nix run .`
        defaultApp = flake.apps."cardano-mq-sync:exe:cardano-mq-sync";

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
        }
        # nix run .#<exe>
        // (collectExes flake.apps);
      }
    ) // {
      overlay = import ./overlay.nix self;
    };
}
