# nix-shell entry: GHC 9.12 with every dependency, cabal, and curl.
# The same environment as `nix develop`, from the same nixpkgs revision:
# it reads the lock file so both entries hit the binary cache identically.
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  node = lock.nodes.nixpkgs.locked;
  pinned = builtins.fetchTarball {
    url = "https://github.com/${node.owner}/${node.repo}/archive/${node.rev}.tar.gz";
    sha256 = node.narHash;
  };
in
{ pkgs ? import pinned { } }:
let
  hs = pkgs.haskell.packages.ghc912;
  ghc = hs.ghcWithPackages (p: with p; [
    aeson aeson-pretty scientific vector containers text bytestring
    process directory filepath optparse-applicative
  ]);
in pkgs.mkShell {
  packages = [ ghc hs.cabal-install pkgs.curl ];
}
