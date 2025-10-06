{
  description = "An attempt at a simple nix dev shell and build script";
  inputs.nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs-unstable }:
    let
      # Support multiple systems
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs-unstable.lib.genAttrs systems;
      pkgsFor = system: import nixpkgs-unstable { inherit system; };
    in
    {
      # Define packages
      packages = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.haskellPackages.developPackage {
            root = ./.;
            name = "gcalc";
          };
        }
      );

      # Define development shell
      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              # Add Haskell development tools
              haskellPackages.ghc
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              haskellPackages.hlint
              haskellPackages.ormolu
            ];
          };
        }
      );
    };
}