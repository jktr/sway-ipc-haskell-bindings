{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  outputs = { self, nixpkgs }:
  with nixpkgs.lib;
  let
    systems = platforms.unix;
    forAllSystems = fn: (genAttrs systems (system:
      fn (import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
        ];
      })
    ));
    getHaskellPackages = pkgs: pattern: pipe pkgs.haskell.packages [
      attrNames
      (filter (x: !isNull (strings.match pattern x)))
      (sort (x: y: x>y))
      (map (x: pkgs.haskell.packages.${x}))
      head
    ];
  in {
    packages = forAllSystems (pkgs: let
      ghc92 = getHaskellPackages pkgs "ghc92.";
    in rec {
      default = sway-ipc;
      sway-ipc = ghc92.sway-ipc;
    });

    overlays = {
      default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
            sway-ipc = hfinal.callCabal2nix "sway-ipc" ./sway-ipc {};
          };
        };
      };
    };

    devShells = forAllSystems (pkgs:
      let
        haskellPackages = getHaskellPackages pkgs "ghc92.";
      in rec {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.sway-ipc
          ];
          nativeBuildInputs = [
            haskellPackages.haskell-language-server
            pkgs.cabal-install
            pkgs.hlint

            # in addition, for ghcid-wrapper
            pkgs.zsh
            pkgs.entr
            pkgs.ghcid
          ];
        };
      }
    );
  };
}
