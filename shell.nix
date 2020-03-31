let pkgs = import ./. {};
  exe = name: pkgs.hsPkgs.${name}.components.exes.${name};
in
pkgs.hsPkgs.shellFor {
  buildInputs = [(exe "doctest") (exe "hsc2hs")];
}
