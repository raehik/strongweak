{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # If you have a .cabal file in the root, this option is determined
          # automatically. Otherwise, specify all your local packages here.
          # packages.example.root = ./.;

          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          basePackages = pkgs.haskell.packages.ghc96;

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };
          overrides = self: super: with pkgs.haskell.lib; {
            # 2023-04-17 raehik: hourglass tests broken from GHC 9.2.5
            # PR: https://github.com/vincenthz/hs-hourglass/pull/56
            hourglass = dontCheck super.hourglass;

            # 2023-04-17 raehik: need hedgehog 1.2 for GHC 9.6
            hedgehog = super.hedgehog_1_2;
            tasty-hedgehog = super.tasty-hedgehog_1_4_0_1;

            # 2023-04-17 raehik: retry: tests broken
            # PR: https://github.com/Soostone/retry/pull/82
            retry = dontCheck super.retry;

            # 2023-04-17 raehik: warp: need new for GHC 9.6 (unix-2.8)
            # also has 3 test failures. idk why. disabling
            # also has friends that need swapping out. heck on earth
            warp = dontCheck super.warp_3_3_25;
            recv = super.recv_0_1_0;
            warp-tls = super.warp-tls_3_3_6;

            # 2023-04-17 raehik: bsb-http-chunked: tests broken
            # maybe problematic type wildcard usage...?
            bsb-http-chunked = dontCheck super.bsb-http-chunked;

            # 2023-04-17 raehik: finite-typelits needs base upper bound bump
            # idk how to do a Cabal file patch. so jailbreak it is.
            # PR: https://github.com/mniip/finite-typelits/pull/22
            finite-typelits = doJailbreak super.finite-typelits;

            # 2023-04-17 raehik: refined: need new
            refined = super.refined_0_8_1;
          };

          devShell = {
            tools = hp: {
              ghcid = null; # broken on GHC 9.6? old fsnotify
              hlint = null; # broken on GHC 9.6? old
            };
          #  # Enabled by default
          #  enable = true;  
          #
          #  # Programs you want to make available in the shell.
          #  # Default programs can be disabled by setting to 'null'
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #
          #  hlsCheck.enable = true;
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.strongweak;
      };
    };
}
