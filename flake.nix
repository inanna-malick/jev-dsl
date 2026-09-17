{
  description = "jev-dsl: GHC 9.12, cabal, and curl for the examples";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      devShells = forAll (pkgs:
        let
          hs = pkgs.haskell.packages.ghc912;
          # Every dependency of the library, the executables, and the test suite,
          # so cabal builds without fetching from Hackage.
          ghc = hs.ghcWithPackages (p: with p; [
            aeson aeson-pretty scientific vector containers text bytestring
            process directory filepath optparse-applicative
          ]);
        in {
          default = pkgs.mkShell {
            packages = [ ghc hs.cabal-install pkgs.curl ];
          };
        });
    };
}
