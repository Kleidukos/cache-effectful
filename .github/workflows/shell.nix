{ ghcVersion }:
let pkgs = import <nixpkgs> {};
in with pkgs;
  mkShell {
    buildInputs = [
      # Haskell Deps
      haskell.compiler."ghc${ghcVersion}"
      cabal-install
      hlint
      haskellPackages.apply-refact
      stylish-haskell

      # DB Deps
      postgresql_13
      gmp
      zlib
      glibcLocales

      # Extra
      parallel
      git
      # mkdocs
      gnumake
    ];
    shellHook = ''
      export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
      export LC_ALL=C.UTF-8
    '';
  }
