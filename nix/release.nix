let pkgs = import (../default.nix);
in rec {
  wolf-static = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.wolf;
  release-target = pkgs.stdenv.mkDerivation {
      name = "wolf-release";
      buildInputs = [ wolf-static ];
      nativeBuildInputs =
           pkgs.lib.attrsets.attrValues pkgs.wolfPackages;
      buildCommand = ''
        cp -r ${wolf-static} $out
      '';
    };
}
