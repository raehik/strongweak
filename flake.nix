# TODO
# * better devshell name overriding. clumsy because we can't access the
#   derivation being used (because it's auto-grabbed). really just wanna change
#   `ghc-shell-for` to `ghcXY` and keep the `-${pname}-${version}`!
# * honestly maybe I move away from haskell-flake...? it's weird
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    rerefined.url   = "github:raehik/rerefined";
    rerefined.flake = false;
    type-level-show.url   = "github:raehik/type-level-show";
    type-level-show.flake = false;
    singleraeh.url   = "github:raehik/singleraeh";
    singleraeh.flake = false;
    linear-builder.url   = "github:Bodigrim/linear-builder";
    linear-builder.flake = false;
  };
  outputs = inputs:
  let
    # simple devshell for non-dev compilers: really just want `cabal repl`
    nondevDevShell = compiler: {
      mkShellArgs.name = "${compiler}-strongweak";
      hoogle = false;
      tools = _: {
        hlint = null;
        haskell-language-server = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-strongweak;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          packages.rerefined.source = inputs.rerefined;
          packages.type-level-show.source = inputs.type-level-show;
          packages.singleraeh.source = inputs.singleraeh;
          packages.text-builder-linear.source = inputs.linear-builder;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.rerefined.source = inputs.rerefined;
          packages.type-level-show.source = inputs.type-level-show;
          packages.singleraeh.source = inputs.singleraeh;
          packages.text-builder-linear.source = inputs.linear-builder;
          devShell.mkShellArgs.name = "ghc96-strongweak";
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.rerefined.source = inputs.rerefined;
          packages.type-level-show.source = inputs.type-level-show;
          packages.singleraeh.source = inputs.singleraeh;
          packages.text-builder-linear.source = inputs.linear-builder;
          devShell = nondevDevShell "ghc94";
        };
      };
    };
}
