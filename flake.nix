{
  description = "Moore Machine Based LLM Agents";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    flake-utils.url = github:numtide/flake-utils;
    monoidal-functors = {
      url = github:solomon-b/monoidal-functors/solomon/removes-overlay;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, monoidal-functors }:
    let
      ghcVersion = "963";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let pkgs = import nixpkgs { inherit system; };
            hsPkgs = pkgs.haskell.packages.${compiler}.override {
              overrides = hfinal: hprev: {
                agent-moore = (hfinal.callCabal2nix "agent-moore" ./. { });
                bifunctors = hfinal.bifunctors_5_6_1;
                monoidal-functors = hfinal.callCabal2nix "monoidal-functors" monoidal-functors {};
                semigroupoids = hfinal.semigroupoids_6_0_0_1;
              };
            };
        in rec {
          devShells.default = hsPkgs.shellFor {
            packages = p: [ p.agent-moore ];
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskell.compiler.${compiler}
              pkgs.haskell.packages.${compiler}.haskell-language-server
              pkgs.nixfmt

              pkgs.pkg-config
            ];
          };
          formatter = pkgs.nixpkgs-fmt;
          packages.default = hsPkgs.agent-moore;
        });
}
