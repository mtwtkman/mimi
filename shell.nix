with import <nixpkgs> {};
mkShell {
    packages = [
        ghc
        python310
        nodejs
    ];
}