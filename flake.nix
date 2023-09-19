{
  description = "My Haskell.nix Project";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    scrypt.url = "github:SupercedeTech/scrypt";
  };

  outputs = { self, haskellNix, nixpkgs, flake-utils, CHaP,scrypt }:
    flake-utils.lib.eachDefaultSystem (system:
     let
        # Fetch the custom scrypt source
        customScryptSrc = scrypt {
          owner = "SupercedeTech";
          repo = "scrypt";
          rev = "master"; 
          sha256 = "6409ede122aff2fd7f8746a469ef01fdfa56cbc4"; # Replace with actual sha256
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # Overriding the default scrypt package
            scrypt = prev.scrypt.overrideAttrs (oldAttrs: {
              src = customScryptSrc;
            });
            # This overlay adds our project to pkgs
            plutus-test =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = "3.4.1";
                  haskell-language-server = rec {
                    # HLS fix for stm-hamt bug
                    src = final.haskell-nix.sources."hls-1.10";
                    cabalProject = __readFile (src + "/cabal.project");
                  };
                };
                # Non-Haskell applications required by your project
                shell.buildInputs = with pkgs; [
                  bashInteractive
                  gnugrep
                  nixpkgs-fmt
                  nix-prefetch-git
                  nodejs-18_x
                  (vscode-with-extensions.override {
                    vscode = pkgs.vscodium;
                    vscodeExtensions = with pkgs.vscode-extensions; [
                      haskell.haskell
                      jnoortheen.nix-ide
                      justusadam.language-haskell
                      mkhl.direnv
                    ];
                  })
                ];
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.plutus-test.flake { };
      in
      flake // {
        # Built by `nix build .`
        packages.default = flake.packages."plutus-test:exe:plutus-test";
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}