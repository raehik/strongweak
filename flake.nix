{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    refined1.url = "github:raehik/refined/refined1-hackage";
    refined1.flake = false;
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-strongweak;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          packages.refined1.source = inputs.refined1;
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.refined1.source = inputs.refined1;
          devShell.mkShellArgs.name = "ghc96-strongweak";
        };
      };
    };
}
