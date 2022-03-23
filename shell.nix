with import <nixpkgs> {};
mkShell {
    packages = [
        elmPackages.elm
        elmPackages.elm-format
        elmPackages.elm-test
        ghc
        python310
    ];
}