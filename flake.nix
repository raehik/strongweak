{
  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    refined = {
      url = "github:raehik/refined/refined1";
      flake = false;
    };
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          #haskellPackages = pkgs.haskell.packages.ghc925;
          packages = {
            strongweak.root = ./.;
          };
          # buildTools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          overrides = self: super: {
            #refined = self.callCabal2nix "refined" inputs.refined {};
          };
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
      };
    };
}
