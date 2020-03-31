let 
  # Fetch the latest haskell.nix and import its default.nix 
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these. 
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
, haskellCompiler ? "ghc865"
}:
pkgs // {
  hsPkgs = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "arrays";
      src = ./.;
    };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [{
      nonReinstallablePkgs = [
        "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
        "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
        # ghcjs custom packages
        "ghcjs-prim" "ghcjs-th"
            "ghc-boot"
        "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
        "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
        # "ghci" "haskeline"
        "hpc"
        "mtl" "parsec" "process" "text" "time" "transformers"
        "unix" "xhtml"
        "stm" "terminfo"
    ];
    }];
    pkg-def-extras = [(hackage: {
      base-compat = hackage.base-compat."0.7.0".revisions.default;
      code-page = hackage.code-page."0.1.3".revisions.default;
      doctest = hackage.doctest."0.16.2".revisions.default;
      ghc-paths = hackage.ghc-paths."0.1.0.9".revisions.default;
      hsc2hs = hackage.hsc2hs."0.68.7".revisions.default;
      process = hackage.process."1.6.8.2".revisions.default;
      setenv = hackage.setenv."0.1.1.3".revisions.default;
      syb = hackage.syb."0.7.1".revisions.default;
    })];
  };
}
