{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  outputs = { self, nixpkgs }:
    with nixpkgs.legacyPackages.x86_64-linux;
    let
      hl = haskell.lib;
      hsPkgs = haskell.packages.ghc94;

      extraHsPkgs = hsPkgs.override {
        overrides = self: super: {
          ListLike = hl.dontCheck super.ListLike;
          hlint = super.hlint_3_5;
        };
      };

      hsPkgsFn = p: with p; [ tasty-hunit tasty-quickcheck tasty-th ];
    in {
      devShell.x86_64-linux = mkShell {
        buildInputs = with extraHsPkgs; [
          (ghcWithPackages hsPkgsFn)
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
        ];
      };
    };
}
