{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    refined1 = {
      url = "github:raehik/refined/refined1-hackage";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, config, ... }: {

        haskellProjects.ghc96 = import ./haskell-flake-ghc96.nix pkgs;

        haskellProjects.default = {
          basePackages = config.haskellProjects.ghc96.outputs.finalPackages;

          packages = {
            refined1.source = inputs.refined1;
          };

          devShell = {
            tools = hp: {
              ghcid = null; # broken on GHC 9.6? old fsnotify
              hlint = null; # broken on GHC 9.6? old
              haskell-language-server = null; # TAKES AGES TO BUILD FFS
            };
          };

        };

        packages.default = self'.packages.strongweak;
      };
    };
}
